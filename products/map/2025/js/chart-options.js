/**
 * Chart options.
 * 
 *
 * @summary Chart options called in map.js.
 * 
 */



// define canvases
const ctx1 = document.getElementById("econ1Canv").getContext("2d");
const ctx2 = document.getElementById("econ2Canv").getContext("2d");
const ctx3 = document.getElementById("eduCanv").getContext("2d");
const ctx4 = document.getElementById("povCanv").getContext("2d");
const ctx5 = document.getElementById("segCanv").getContext("2d");
const ctx6 = document.getElementById("change1Canv").getContext("2d");
const ctx7 = document.getElementById("change1bCanv").getContext("2d");
const ctx8 = document.getElementById("change2Canv").getContext("2d");
const ctx9 = document.getElementById("change3Canv").getContext("2d");



// threshold shape
function threshShape(x) {
  // create array with two identical values so that bar is plotted as a line
  return x.map((v) => [v, v]);
};


// x scales
const percentScale = {
  ticks: {
    callback: (value) => {
      return `${value * 100}%`;
    },
    stepSize: 0.25
  },
  min: 0,
  max: 1,
  beginAtZero: true,
  grid: {
    display: true,
  },
};

function changeFlag(nbrhood_chng) {
  if (nbrhood_chng == 1) {
    return "Meets definition for Neighborhood Change";
  } else {
    return "Does not meet definition for Neighborhood Change";
  }
}


const changeScale = {
  ticks: {
    callback: (value) => {
      return `${Math.round(value * 100)}%`;
    },
  },
  suggestedMin: -0.4,
  suggestedMax: 0.4,
  beginAtZero: true,

  grid: {
    display: true,
  },
};

const rentScale = {
  ticks: {
    callback: (value) => {
      return `${Math.round(value * 100)}%`;
    },
  },
  suggestedMin: 0,
  suggestedMax: 1,
  beginAtZero: true,

  grid: {
    display: true,
  },
};


const dollarScale = {
  ticks: {
    callback: (value) => {
      return value < 1000000
        ? "$" + value / 1000 // + "K"
        : "$" + value / 1000000 + "M";
    },
    stepSize: 1000000,
  },
  suggestedMax: 2000000,
  beginAtZero: true,
  grid: {
    display: true,
  },
};

// tool tip annotation
const tipPercent = {
  callbacks: {
    title: () => null,
    label: (context) => {

      // define tipval
      let tipVal = context.parsed.x * 100;
      
      // threshold prefix
      if(context.raw.length == 2){
        tipVal = "Threshold: " + tipVal.toFixed(1) + "%";
      } 
      // tract prefix
      if(context.raw.length != 2 && context.dataset.data.slice(-1) != 'Rural Areas') {
        tipVal = "Tract: " + tipVal.toFixed(1) + "%";
      }
      // block group prefix
      if(context.raw.length != 2 && context.dataset.data.slice(-1) == 'Rural Areas') {
        tipVal = "Block Group: " + tipVal.toFixed(1) + "%";
      }
      return tipVal;
    },
  },
  backgroundColor: '#969696',
  cornerRadius: 1.5,
  boxHeight: 6,
  displayColors: false

};


const tipDollar = {
  callbacks: {
    title: () => null,
    // label: (context) => {
    //   let tipVal = context.parsed.x;
    //   tipVal = "$" + tipVal.toLocaleString();
    label: (context) => {

      // define tipval
      let tipVal = context.parsed.x;
      
      // threshold prefix
      if(context.raw.length == 2){
        tipVal = "Threshold: $" + tipVal.toLocaleString();
      } 
      // tract prefix
      if(context.raw.length != 2 && context.dataset.data.slice(-1) != 'Rural Areas') {
        tipVal = "Tract: $" + tipVal.toLocaleString();
      }
      // block group prefix
      if(context.raw.length != 2 && context.dataset.data.slice(-1) == 'Rural Areas') {
        tipVal = "Block Group: $" + tipVal.toLocaleString();
      }

      return tipVal;
    },
  },
  backgroundColor: '#969696',
  cornerRadius: 1.5,
  boxHeight: 6,
  displayColors: false
};


// chart annotation
annoNulls = (arr) => {
  // loop to get positions of nulls
  var indices = [], i;
  for (i = 0; i < arr.length; i++)
    if (arr[i] === undefined)
      indices.push(i);


  if (indices.length < 1) {
    // if no nulls return null for anno
    return null

  } else {
    // proceed with creating annotations

    let anno = {}
    let config = {}

    config = {
      // config without y value
      xValue: 0,
      xAdjust: 30,
      type: 'label',
      content: ['N/A'],
      backgroundColor: '#ffffff',
      color: '#969696',
      textAlign: 'left',
      padding: 1.25,
      font: {
        size: 8
      },
      callout: {
        display: true,
        side: 10,
        borderColor: '#969696',

      }
    }

    // add yvalue itteratively by spreading the config object
    if (indices.length == 1) {
      anno = { lab0: { ...config, yValue: indices[0] } }
      return anno

    } else if (indices.length == 2) {
      anno = { lab0: { ...config, yValue: indices[0] }, lab1: { ...config, yValue: indices[1] } }
      return anno

    } else if (indices.length == 3) {
      anno = {
        lab0: { ...config, yValue: indices[0] }, lab1: { ...config, yValue: indices[1] },
        lab2: { ...config, yValue: indices[2] }
      }
      return anno

    } else {
      anno = {
        lab0: { ...config, yValue: indices[0] }, lab1: { ...config, yValue: indices[1] },
        lab2: { ...config, yValue: indices[2] }, lab3: { ...config, yValue: indices[3] }
      }
      return anno

    }
  }
}

// chart options
const econ1Options = {
  layout: {
    padding: {
      left: 23
    }
  },
  responsive: true,
  indexAxis: "y",
  plugins: {
    maintainAspectRatio: false,
    tooltip: tipPercent,
    legend: {
      display: false,
    },
  },
  // plot bar and regional median
  scales: {
    y: {
      // drawBorder: true,
      ticks: {
        stepSize: 1,
        min: 0,
        autoSkip: false,
      },
      stacked: false,
      grid: {
        display: false,
      },
    },
    y2: {
      display: false,
    },
    x: percentScale,
  },
};

const econ2Options = {
  layout: {
    padding: {
      left: 35,
    }
  },
  responsive: true,
  indexAxis: "y",
  plugins: {
    maintainAspectRatio: false,
    tooltip: tipDollar,
    legend: {
      display: false,
    },
  },
  scales: {
    y: {
      stacked: false,
      grid: {
        display: false,
      },
    },
    y2: {
      display: false,
    },
    x: dollarScale,
  },
};

const eduOptions = {
  responsive: true,
  indexAxis: "y",
  plugins: {
    maintainAspectRatio: false,
    tooltip: tipPercent,
    legend: {
      display: false,
    },
  },
  // plot bar and regional median
  scales: {
    y: {
      ticks: {
        stepSize: 1,
        min: 0,
        autoSkip: false,
      },
      stacked: false,
      grid: {
        display: false,
      },
    },
    y2: {
      display: false,
    },
    x: percentScale,
  },
};


// pov options
const povOptions = {
  layout: {
    padding: {
      left: 70
    }
  },
  responsive: true,
  indexAxis: "y",
  plugins: {
    maintainAspectRatio: false,
    tooltip: tipPercent,
    legend: {
      display: false,
    },
  },
  // plot bar and regional median
  scales: {
    y: {
      ticks: {
        stepSize: 1,
        min: 0,
        autoSkip: false,
        crossAlign: "far",
      },
      stacked: false,
      grid: {
        display: false,
      },
    },
    y2: {
      display: false,
    },
    x: percentScale,
  },
};


const segOptions = {
  layout: {
    padding: {
      left: 98
    }
  },
  responsive: true,
  indexAxis: "y",
  plugins: {
    maintainAspectRatio: false,
    tooltip: tipPercent,
    legend: {
      display: false,
    },
  },
  scales: {
    y: {
      ticks: {
        stepSize: 1,
        min: 0,
        autoSkip: false,
       
      },
      stacked: false,
      grid: {
        display: false,
      },
    },
    y2: {
      display: false,
    },
    x: percentScale,
  },
};

// change options
const changeOptions = {
  layout: {
    padding: {
      left: 40
    }
  },
  responsive: true,
  indexAxis: "y",
  plugins: {
    maintainAspectRatio: false,
    tooltip: tipPercent,
    legend: {
      display: false,
    },
  },
  // plot bar and regional median
  scales: {
    y: {
      ticks: {
        min: 0,
        autoSkip: false,
        crossAlign: "far",
      },
      stacked: false,
      grid: {
        display: false,
      },
    },
    y2: {
      display: false,
    },
    x: changeScale,
  },
};


const rentChangeOptions = {
  layout: {
    padding: {
      left: 51
    }
  },
  responsive: true,
  indexAxis: "y",
  plugins: {
    maintainAspectRatio: false,
    tooltip: tipPercent,
    legend: {
      display: false,
    },
  },
  // plot bar and regional median
  scales: {
    y: {
      ticks: {
        min: 0,
        autoSkip: false,
        crossAlign: "far",
      },
      stacked: false,
      grid: {
        display: false,
      },
    },
    y2: {
      display: false,
    },
    x: rentScale,
  },
};

const gapChangeOptions = {
  layout: {
    padding: {
      left: 52
    }
  },
  responsive: true,
  indexAxis: "y",
  plugins: {
    maintainAspectRatio: false,
    tooltip: tipPercent,
    legend: {
      display: false,
    },
  },
  // plot bar and regional median
  scales: {
    y: {
      ticks: {
        min: 0,
        autoSkip: false,
      },
      stacked: false,
      grid: {
        display: false,
      },
    },
    y2: {
      display: false,
    },
    x: rentScale,
  },
};





