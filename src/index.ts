'use strict';
import { Elm } from "./Main.elm";
import Fuse from "fuse.js";
import { Entry, Projects } from "./Hovercraft";

var app = Elm.Main.init({ node: document.getElementById("root") });

const fuseOptions = {
	includeScore: true,
	// sortFn: (a: { score: number; }, b: { score: number; }) => { return a.score - b.score },
	keys: ['hover.contents.value'],
};

const fuseList: Entry[] = [];

let fuse = new Fuse(fuseList, fuseOptions);

const projectIds = [];

fetch('/hovercraft')
	.then(res => res.json())
	.then(res => { console.log(res); return res; })
	.then((projects: Projects) => {
		for (let projectId in projects) {
			const hovercrafts = projects[projectId];
			projectIds.push(projectId);
			for (let page of hovercrafts.pages) {
				for (let entry of page.entries) {
					fuseList.push(entry);
				}
			}
		}
		fuse = new Fuse(fuseList, fuseOptions);
		app.ports.projectIdsReceiver.send(projectIds);
	});

app.ports.requestSearch.subscribe(function({ placeholder, isFuzzMode, projectIds, query }) {
	console.log("requestSearch", isFuzzMode, query);

	const projectIdParams = projectIds.map(projectId => `projectIds[]=${projectId}`).join('&');

	if (isFuzzMode) {
		const rawResults = fuse.search(query);
		console.log("rawResults", rawResults);
		const results: { hit: Entry, score: number }[] = rawResults
			.filter(entry => projectIds.includes(entry.item.projectId))
			.map(entry => { return { "hit": entry.item, "score": entry.score } });
		app.ports.searchReceiver.send([query, results]);
	} else {
		fetch('/search?placeholder=' + placeholder + '&' + projectIdParams + '&q=' + query)
			.then(res => res.json())
			.then(res => {
				console.log("search", res);
				return res;
			})
			.then(res => app.ports.searchReceiver.send([res.query, res.results]))
	}
});
