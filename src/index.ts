'use strict';
import { Elm } from "./Main.elm";
import { Document } from "flexsearch";
import * as lsp from "vscode-languageserver-types";

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

interface Projects {
	[projectName: string]: Hovercraft;
}

type Hovercraft = Array<Page>;

interface Page {
	document: lsp.TextDocumentIdentifier;
	entries: Array<Entry>;
}

interface Entry {
	hover: lsp.Hover;
	definition: { uri: lsp.URI, range: lsp.Range };
	moniker: any;
	rootPath: string;
}

fetch('/hovercraft')
	.then(res => res.json())
	.then((projects: Projects) => {
		let id = 0;
		for (let projectName in projects) {
			const hovercrafts = projects[projectName];
			for (let page of hovercrafts) {
				for (let entry of page.entries) {
					index.add({ id: id, contents: entry });
					id++;
				}
			}
		}
	});

app.ports.requestSearch.subscribe(function (query) {
	console.log("requestSearch", query);

	const results = index.search(query, { pluck: "contents:hover:contents:value", enrich: true }).map(function (result: any) {
		return result.doc.contents;
	});
	console.log("results", results);
	app.ports.searchReceiver.send(results);
});
