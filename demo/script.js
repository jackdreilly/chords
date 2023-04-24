import htm from 'https://esm.sh/htm';
import { h, render } from 'https://esm.sh/preact';
import { useState, useMemo } from 'https://esm.sh/preact/hooks';
import init, * as chords from './pkg/chords.js';
await init();

const html = htm.bind(h);

function App() {
    const [searchTerm, setSearchTerm] = useState("");
    const [transpose, setTranspose] = useState(0);
    const parse = useMemo(() => chords.parse(searchTerm, transpose), [searchTerm, transpose]);
    let [letter, note, chord, scale] = [undefined, undefined, undefined, undefined];
    if (parse?.letter) {
        letter = html`
        <div class="shadow rounded m-4 p-4 inline-block">
            <h2 class="text-xl m-2">Letter</h2>
            <p>${parse.letter}</p>
        </div>
        `;
    }
    if (parse?.note) {
        note = html`
        <div class="shadow rounded m-4 p-4 inline-block">
            <h2 class="text-xl m-2">Note</h2>
            <p class="p-2">${parse.note.letter}</p><p class="p-2">${parse.note.accidental}</p>
        </div>
        `
    }
    if (parse?.chord) {
        const bass = parse.chord.bass ? html`
        <div>
                <h3 class="text-lg m-1">Bass</h3>
        <p class="p-2">${parse.chord.bass.letter}</p><p class="p-2">${parse.chord.bass.accidental}</p>
        </div>
        ` : undefined;
        let augmentations = undefined;
        if (parse.chord.augmentations?.length) {
            const lis = parse.chord.augmentations.map(augmentation => html`<li><p class="p-2">${augmentation.interval}</p><p>${augmentation.accidental}</p></li>`);
            augmentations = html`
            <div>
                <h3 class="text-lg m-1">augmentations</h3>
                <ul class="p-2">${lis}</ul>
            </div>
            `;
        }
        chord = html`
        <div class="shadow rounded m-4 p-4 inline-block">
            <h2 class="text-xl m-2">Chord</h2>
            <div>
                <h3 class="text-lg m-1">Root</h3>
                <p class="p-2">${parse.chord.root.letter}</p><p class="p-2">${parse.chord.root.accidental}</p>
            </div>
            <div>
                <h3 class="text-lg m-1">Type</h3>
                <p class="p-2">${parse.chord.chord_type}</p>
            </div>
            ${augmentations}
            ${bass}
            <div>
                <h3 class="text-lg m-1">Notes</h3>
                <div>
                    ${parse.chord_notes.join(" ")}
                </div>
            </div>
        </div>
        `;
    }
    if (parse?.scale) {
        scale = html`
        <div class="shadow rounded m-4 p-4 inline-block">
            <h2 class="text-xl m-2">Scale</h2>
            <div>
                <h3 class="text-lg m-1">Root</h3>
                <p class="p-2">${parse.scale.root.letter}</p><p class="p-2">${parse.scale.root.accidental}</p>
            </div>
            <div>
                <h3 class="text-lg m-1">Mode</h3>
                <p class="p-2">${parse.scale.mode}</p>
            </div>
            <div>
                <h3 class="text-lg m-1">Notes</h3>
                <div>
                    ${parse.scale_notes.join(" ")}
                </div>
            </div>
        </div>
        `;
    }
    let accidental = undefined;
    if (parse?.accidental) {
        accidental = html`
        <div class="shadow rounded m-4 p-4 inline-block">
            <h2 class="text-xl m-2">Accidental</h2>
            <p>${parse.accidental}</p>
        </div>
        `;
    }
    let augmentation = undefined;
    if (parse?.augmentation) {
        augmentation = html`
        <div class="shadow rounded m-4 p-4 inline-block">
            <h2 class="text-xl m-2">Augmentation</h2>
            <p class="p-1">${parse.augmentation.interval}</p><p>${parse.augmentation.accidental}</p>
        </div>
        `;
    }
    let mode = undefined;
    if (parse?.mode) {
        mode = html`
        <div class="shadow rounded m-4 p-4 inline-block">
            <h2 class="text-xl m-2">Mode</h2>
            <p>${parse.mode}</p>
        </div>
        `;
    }
    let chord_type = undefined;
    if (parse?.chord_type) {
        chord_type = html`
        <div class="shadow rounded m-4 p-4 inline-block">
            <h2 class="text-xl m-2">Chord Type</h2>
            <p>${parse.chord_type}</p>
        </div>
        `;
    }

    return html`
    <div class="p-4 h-full w-full bg-gray-100 font-mono">
        <h1 class="text-2xl m-2">Music Parser</h1>
        <div class="m-2 p-2">
            <a href="https://github.com/jackdreilly/chords" class="font-medium text-blue-600 dark:text-blue-500 hover:underline">Github Repo for Rust Code</a>
        </div>
        <div>
            <input class="m-2 max-w-sm bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                id="search" autofocus placeholder="Chord or Scale or..." type="text" onInput=${e => setSearchTerm(e.target.value)} />
            <input class="m-2 max-w-sm bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                id="transpose" placeholder="Transpose" type="number" onInput=${e => setTranspose(e.target.valueAsNumber)} />
            
        </div>
        <div class="shadow rounded m-4 p-4 inline-block bg-white">
        <h1 class="text-2xl m-4">Results</h1>
        ${chord}
        ${scale}
        ${letter}
        ${note}
        ${augmentation}
        ${accidental}
        ${chord_type}
        ${mode}
        </div>
    </div>
    `;
}

render(html`<${App} />`, document.body);