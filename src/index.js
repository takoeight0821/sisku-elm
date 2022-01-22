'use strict';
import { Elm } from "./Main.elm";
import { Document } from "flexsearch";
import hovercrafts from "../data/hovercraft.json";

var app = Elm.Main.init({ node: document.getElementById("root") });

const index = new Document({
	encode: function (str) {
		return str.toLowerCase().split(/\s+/g);
	},
	tokenize: "forward",
	document: {
		id: "id",
		index: ["contents:hover:contents:value"],
	}
});

for (let i = 0; i < hovercrafts.length; i++) {
	index.add({ id: i, contents: hovercrafts[i] });
}

app.ports.requestSearch.subscribe(function (query) {
	console.log("requestSearch", query);

	const results = index.search(query, { pluck: "contents:hover:contents:value" })
		.map((id) => hovercrafts[id]);
	console.log("results", results);
	app.ports.searchReceiver.send(results);
});
