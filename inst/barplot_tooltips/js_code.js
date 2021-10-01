// TEST for BAR PLOTS run in dev tools console
// plot inst/barplot_tooltips/svg_output/p1.svg
// data inst/barplot_tooltips/json_output/singlelayer_1.json
{
const data = {"bar":[{"tooltip":"<ul><\/ul>","coordX":0.1162,"coordY":0.8141,"type":"bar"},{"tooltip":"<ul><\/ul>","coordX":0.1162,"coordY":0.347,"type":"bar"},{"tooltip":"<ul><\/ul>","coordX":0.1162,"coordY":0.2302,"type":"bar"},{"tooltip":"<ul><\/ul>","coordX":0.3585,"coordY":0.7557,"type":"bar"},{"tooltip":"<ul><\/ul>","coordX":0.3585,"coordY":0.5222,"type":"bar"},{"tooltip":"<ul><\/ul>","coordX":0.3585,"coordY":0.4638,"type":"bar"},{"tooltip":"<ul><\/ul>","coordX":0.6007,"coordY":0.1719,"type":"bar"},{"tooltip":"<ul><\/ul>","coordX":0.6007,"coordY":0.0551,"type":"bar"}]};

function isWhite(color) {
    return !color || color.match(/#ffffff|rgba?\(255,\s*255,\s*255|transparent|white/i);
}

// function return new array that is shuffled with Fisher-Yates algorith
const shuffleArray = array => {
  array = array.slice();
  if (array.length === 0) {
    return array;
  }
  let i = array.length;
  while (--i) {
    const j = Math.floor(Math.random() * (i + 1));
    const temp = array[i];
    array[i] = array[j];
    array[j] = temp;
  }
  return array;
}

// return array with removed duplicates
// it's adviced to use only on primitives
const unique = (arr) => [...new Set(arr)];

const split = (string) => string.split(/[\s,.]+/);

// returns a function that return random word from a given array
const randomize = (array, tmp = []) => () => {
  if (tmp.length === 0) {
    tmp = shuffleArray(array);
  }
  return tmp.shift();
}


const words = unique(split("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus posuere volutpat ligula, vel gravida purus accumsan non. Fusce cursus vehicula ligula, ac pretium urna ultricies vel. Fusce ultrices purus nibh, et porta arcu lobortis non. Maecenas porta orci in arcu cursus, ut facilisis elit dictum. Vestibulum risus ante, faucibus ac lorem non, eleifend vulputate nisl. Ut pharetra nisl id ante imperdiet dignissim. Morbi in elit sed lectus commodo feugiat et sit amet est."));
const randomWord = randomize(words);

const rect = [...document.querySelectorAll('rect')].filter(rect => {
    return !isWhite(rect.style.fill);
});

const getStyle = name => {
    const re = new RegExp(name + ':\\s*([^;]+)', 'i');
    return element => element.getAttribute('style').match(re)?.[1];
};
const getAttrOrStyle = name => {
    const getStyleProp = getStyle(name);
    return element => element.getAttribute(name) || getStyleProp(element);
};

const getFill = getAttrOrStyle('fill');

const svg = $('svg');

// additional code is for generating random tooltip data (that was missing in JSON)
// and get rectangle colors

$('svg').closest('.shiny-html-output').ggtips({
    type: 'bar',
    colors: rect.map(getFill).filter(color => color != '#EBEBEB'),
    data: {
        points: [data.bar.map(x => ({...x, tooltip: "<ul><li>" + randomWord() + "</li></ul>"}))]
    }
})
}
