'use strict';
import { Elm } from "./Main.elm";
import { Document } from "flexsearch";
import * as hovercrafts from "../data/hovercraft.json";

var app = Elm.Main.init({ node: document.getElementById("root") });

const index = new Document({
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

let id = 0;
for (let page of hovercrafts) {
	for (let entry of page.entries) {
		index.add({ id: id, contents: entry });
		id++;
	}
}

app.ports.requestSearch.subscribe(function (query) {
	console.log("requestSearch", query);

	const results = index.search(query, { pluck: "contents:hover:contents:value", enrich: true }).map(function (result: any) {
		return result.doc.contents;
	});
	console.log("results", results);
	app.ports.searchReceiver.send(results);
});
