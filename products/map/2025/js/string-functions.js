/**
 * Dynamic string results.
 * 
 *
 * @summary Named functions for creating dynamic string results.
 * 
 */


// Get tract of block group
function tractBgString(fips_bg) {
  if (fips_bg == null) {
    return "tract";
  } else {
    return "block group";
  }
}
function bgFlag(fips_bg) {
  if (fips_bg == null) {
    return "N/A";
  } else {
    return fips_bg;
  }
}

// Excluded geos
function excludeString(fips_bg, density_flag, military_flag, prison_flag) {
  if (density_flag == 1) {
    return (
      "<br><i>This Census " +
      tractBgString(fips_bg) +
      " has been <b>excluded from categorization</b> due to <b>low population</b> (population density below 25 people/mi<sup>2</sup> and total population below 750).</i>"
    );
  } else if (military_flag == 1) {
    return (
      "<br><i>This Census " +
      tractBgString(fips_bg) +
      " has been <b>excluded from categorization</b> due to <b>high share of military population</b> (50 percent or more of the age 16+ population are employed by the armed forces).</i>"
    );
  } else if (prison_flag == 1) {
    return (
      "<br><i>This Census " +
      tractBgString(fips_bg) +
      " has been <b>excluded from categorization</b> due to <b>high share of the population residing in an institutional facility</b> (75 percent or more of the total population reside in an institutional facility).</i>"
    )
  } else {
    return (
      ""
    )
  };
}

// Opportunity
function getOppCat(oppcat, exclude_flag) {
  if (exclude_flag == 1) {
    return "--"
  } else if (oppcat != null) {
    return oppcat;
  } else {
    return "Insufficient data";
  }
}

function scoreString(oppcat, fips_bg, oppscore, exclude_flag, total_valid) {
  if (exclude_flag == 1) {
    return ""
  } else if (oppcat != null) {
    return (
      "<br><i>This Census " +
      tractBgString(fips_bg) +
      " has an opportunity score of " +
      "<b>" +
      oppscore +
      "</b>" +
      " <b>of 9</b>, resulting in an opportunity designation of " +
      "<b>" +
      oppcat +
      "</b>" +
      ".  See details below:</i><br>"
    );
  } else if (total_valid < 7) {
    return (
      "<br><i>This Census " +
      tractBgString(fips_bg) +
      " has <b>insufficient data</b> for assessing neighborhood opportunity.</i>"
    );
  }

}

function envHazBox(env_site_score, oppcat) {
  // set path to elements
  let yesBox = document.getElementById("env-yes-box");
  let noBox = document.getElementById("env-no-box");
  let naBox = document.getElementById("env-na-box");

  // set initial class
  yesBox.setAttribute("class", "yes-box");
  noBox.setAttribute("class", "no-box");
  naBox.setAttribute("class", "na-box");

  // conditionally replace class
  if (env_site_score == 0) {
    yesBox.classList.replace('yes-box', 'yes-box-env-flag');
  }
  if(env_site_score == 1){
    if(oppcat == "Highest Resource"){
      noBox.classList.replace('no-box', 'highest-no-box');
    } else if (oppcat == "High Resource") {
      noBox.classList.replace('no-box', 'high-no-box');
    } else if (oppcat == "Moderate Resource") {
      noBox.classList.replace('no-box', 'mod-no-box');
    } else if (oppcat == "Low Resource") {
      noBox.classList.replace('no-box', 'low-no-box');
    } else if (oppcat == null) {
      noBox.classList.replace('no-box', 'insuff-no-box');
    }

  }
  if(env_site_score == null){
    naBox.classList.replace('na-box', 'na-box-flag');
  }
}



// Poverty & Segregation
function hpsFlag(pov_seg_flag, exclude_flag) {
  if (exclude_flag == 1) {
    return "--"
  } else if (pov_seg_flag == 1) {
    return "Meets definition for High-Poverty & Segregated";
  } else {
    return "Does not meet definition for High-Poverty & Segregated";
  }
}
function hpsString(pov_seg_flag, exclude_flag) {
  if (exclude_flag == 1) {
    return ""
  } else if (pov_seg_flag == 1) {
    return "<br><i>This Census tract <b>meets the definition for High-Poverty & Segregated.</b>  See details below:</i>";
  } else {
    return "<br><i>This Census tract <b> does not meet the definition for High-Poverty & Segregated.</b>  See details below:</i>";
  }
}

// Neighborhood Change
function changeFlag(nbrhood_chng, exclude_flag) {
  if (exclude_flag == 1) {
    return "--"
  } if (nbrhood_chng == 1) {
    return "Meets definition for Neighborhood Change";
  } else {
    return "Does not meet definition for Neighborhood Change";
  }
}

function changeString(path_1a, path_1b, path_2, region, exclude_flag, college_flag) {
  if (exclude_flag == 1) {
    return ""
  } else if (path_1a == 1 && path_1b != 1 && path_2 != 1) {
    return "<br><i>This Census tract <b>meets the Pathway 1A definition.</b>  See details below:</i><br><br>";
  } else if (path_1a != 1 && path_1b == 1 && path_2 != 1) {
    return "<br><i>This Census tract <b>meets the Pathway 1B definition.</b>  See details below:</i><br><br>";
  } else if (path_1a != 1 && path_1b != 1 && path_2 == 1) {
    return "<br><i>This Census tract <b>meets the Pathway 2 definition.</b>  See details below:</i><br><br>";

  } else if (path_1a == 1 && path_1b == 1 && path_2 != 1) {
    return "<br><i>This Census tract <b>meets the Pathway 1A and Pathway 1B definitions.</b>  See details below:</i><br><br>";
  } else if (path_1a == 1 && path_1b != 1 && path_2 == 1) {
    return "<br><i>This Census tract <b>meets the Pathway 1A and Pathway 2 definitions.</b>  See details below:</i><br><br>";
  } else if (path_1a != 1 && path_1b == 1 && path_2 == 1) {
    return "<br><i>This Census tract <b>meets the Pathway 1B and Pathway 2 definitions.</b>  See details below:</i><br><br>";
  } else if (path_1a == 1 && path_1b == 1 && path_2 == 1) {
    return "<br><i>This Census tract <b>meets the Pathway 1 and Pathway 2 definitions.</b>  See details below:</i><br><br>";

  
  } else if (path_1a != 1 && path_1b != 1 && path_2 !=1 && region != "Rural Areas" && college_flag != 1) {
    return "<br><i>This Census tract <b>does not meet the Pathway 1 or Pathway 2 definitions.</b>  See details below:</i><br><br>";
  } else if (region == "Rural Areas") {
    return "<br><i>Rural Areas <b>are not considered</b> in the Neighborhood Change map.</i><br><br>";
  } else if (region != "Rural Areas" && college_flag == 1) {
    return "<br><i>Tracts with high concentrations of college or graduate students <b>are not considered</b> in the Neighborhood Change Map.</i><br><br>";
  }
}


// Pathway 1A baseline - race
function baseline1aBoxRace(baseline_race0021) {
  // set path to elements
  let yesBox = document.getElementById("baseline-yes-box-race-1a");
  let noBox = document.getElementById("baseline-no-box-race-1a");
  let naBox = document.getElementById("baseline-na-box-race-1a");

  // set initial class
  yesBox.setAttribute("class", "yes-box");
  noBox.setAttribute("class", "no-box");
  naBox.setAttribute("class", "na-box");

  // conditionally replace class
  if (baseline_race0021 == 1) {
    yesBox.classList.replace('yes-box', 'change-yes-box');
  } else if (baseline_race0021 == 0) {
    noBox.classList.replace('no-box', 'change-no-box');
  } else {
    naBox.classList.replace('na-box', 'na-box-flag');
  }
}

// Pathway 1A baseline - income
function baseline1aBoxInc(baseline_income0021) {
  // set path to elements
  let yesBox = document.getElementById("baseline-yes-box-inc-1a");
  let noBox = document.getElementById("baseline-no-box-inc-1a");
  let naBox = document.getElementById("baseline-na-box-inc-1a");

  // set initial class
  yesBox.setAttribute("class", "yes-box");
  noBox.setAttribute("class", "no-box");
  naBox.setAttribute("class", "na-box");

  // conditionally replace class
  if (baseline_income0021 == 1) {
    yesBox.classList.replace('yes-box', 'change-yes-box');
  } else if (baseline_income0021 == 0) {
    noBox.classList.replace('no-box', 'change-no-box');
  } else {
    naBox.classList.replace('na-box', 'na-box-flag');
  }
}

// Pathway 1B baseline - race
function baseline1bBoxRace(baseline_race1321) {
  // set path to elements
  let yesBox = document.getElementById("baseline-yes-box-race-1b");
  let noBox = document.getElementById("baseline-no-box-race-1b");
  let naBox = document.getElementById("baseline-na-box-race-1b");

  // set initial class
  yesBox.setAttribute("class", "yes-box");
  noBox.setAttribute("class", "no-box");
  naBox.setAttribute("class", "na-box");

  // conditionally replace class
  if (baseline_race1321 == 1) {
    yesBox.classList.replace('yes-box', 'change-yes-box');
  } else if (baseline_race1321 == 0) {
    noBox.classList.replace('no-box', 'change-no-box');
  } else {
    naBox.classList.replace('na-box', 'na-box-flag');
  }
}

// Pathway 1B baseline - income
function baseline1bBoxInc(baseline_income1321) {
  // set path to elements
  let yesBox = document.getElementById("baseline-yes-box-inc-1b");
  let noBox = document.getElementById("baseline-no-box-inc-1b");
  let naBox = document.getElementById("baseline-na-box-inc-1b");

  // set initial class
  yesBox.setAttribute("class", "yes-box");
  noBox.setAttribute("class", "no-box");
  naBox.setAttribute("class", "na-box");

  // conditionally replace class
  if (baseline_income1321 == 1) {
    yesBox.classList.replace('yes-box', 'change-yes-box');
  } else if (baseline_income1321 == 0) {
    noBox.classList.replace('no-box', 'change-no-box');
  } else {
    naBox.classList.replace('na-box', 'na-box-flag');
  }
}


function proximityBox(halfmile_buffer) {
  // set path to elements
  let yesBox = document.getElementById("proximity-yes-box");
  let noBox = document.getElementById("proximity-no-box");
  let naBox = document.getElementById("proximity-na-box");

  // set initial class
  yesBox.setAttribute("class", "yes-box");
  noBox.setAttribute("class", "no-box");
  naBox.setAttribute("class", "na-box");

  // conditionally replace class
  if (halfmile_buffer == 1) {
    yesBox.classList.replace('yes-box', 'change-yes-box');
  } else if (halfmile_buffer == 0) {
    noBox.classList.replace('no-box', 'change-no-box');
  } else {
    naBox.classList.replace('na-box', 'na-box-flag');
  }
}

// Race seperate for Pathway 2 
function baseline2BoxRace(baseline_race1321) {
  // set path to elements
  let yesBox = document.getElementById("baseline-yes-box-race-2");
  let noBox = document.getElementById("baseline-no-box-race-2");
  let naBox = document.getElementById("baseline-na-box-race-2");

  // set initial class
  yesBox.setAttribute("class", "yes-box");
  noBox.setAttribute("class", "no-box");
  naBox.setAttribute("class", "na-box");

  // conditionally replace class
  if (baseline_race1321 == 1) {
    yesBox.classList.replace('yes-box', 'change-yes-box');
  } else if (baseline_race1321 == 0) {
    noBox.classList.replace('no-box', 'change-no-box');
  } else {
    naBox.classList.replace('na-box', 'na-box-flag');
  }
}

// Income seperate for Pathway 2
function baseline2BoxInc(baseline_income1321) {
  // set path to elements
  let yesBox = document.getElementById("baseline-yes-box-inc-2");
  let noBox = document.getElementById("baseline-no-box-inc-2");
  let naBox = document.getElementById("baseline-na-box-inc-2");

  // set initial class
  yesBox.setAttribute("class", "yes-box");
  noBox.setAttribute("class", "no-box");
  naBox.setAttribute("class", "na-box");

  // conditionally replace class
  if (baseline_income1321 == 1) {
    yesBox.classList.replace('yes-box', 'change-yes-box');
  } else if (baseline_income1321 == 0) {
    noBox.classList.replace('no-box', 'change-no-box');
  } else {
    naBox.classList.replace('na-box', 'na-box-flag');
  }
}