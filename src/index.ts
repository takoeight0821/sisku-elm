'use strict';
import { Elm } from "./Main.elm";
import { Document } from "flexsearch";
import Fuse from "fuse.js";
import { Entry, Projects } from "./Hovercraft";

var app = Elm.Main.init({ node: document.getElementById("root") });

const flexsearchIndex = new Document({
	encode: function (str: string) {
		return str.toLowerCase().split(/\s+/g);
	},
	tokenize: "forward",
	document: {
		id: "id",
		index: ["contents:hover:contents:value"],
		store: true,
	}
});

const fuseOptions = {
	includeScore: true,
	sortFn: (a: { score: number; }, b: { score: number; }) => { return b.score - a.score },
	keys: ['hover.contents.value'],
};

const fuseList: Entry[] = [];

let fuse = new Fuse(fuseList, fuseOptions);

fetch('/hovercraft')
	.then(res => res.json())
	.then((projects: Projects) => {
		let id = 0;
		for (let projectName in projects) {
			const hovercrafts = projects[projectName];
			for (let page of hovercrafts) {
				for (let entry of page.entries) {
					flexsearchIndex.add({ id: id, contents: entry });
					fuseList.push(entry);
					id++;
				}
			}
		}
		fuse = new Fuse(fuseList, fuseOptions);
	});

app.ports.requestSearch.subscribe(function ([isFuzzMode, query]) {
	console.log("requestSearch", isFuzzMode, query);

	if (isFuzzMode) {
		const rawResults = fuse.search(query);
		console.log("rawResults", rawResults);
		const results = rawResults.map(entry => entry.item);
		app.ports.searchReceiver.send(results);
	} else {
		const results = flexsearchIndex.search(query, { pluck: "contents:hover:contents:value", enrich: true }).map(function (result: any) {
			return result.doc.contents;
		});
		console.log("results", results);
		app.ports.searchReceiver.send(results);
	}
});
