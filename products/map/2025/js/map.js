mapboxgl.accessToken = 'pk.eyJ1Ijoiam9zZXBoZGEiLCJhIjoiY20xOWc3Y3RhMDEzaTJrcTMwbjBnMzhsYSJ9.UfpE3U-Tanwbfa0N-Ms9XQ';


const bounds = [
    [
        -143.09375000000045,
        28.601008135311147
    ],
    [
        -100.90625000000081,
        44.56471371814072
    ]]

const map = new mapboxgl.Map({
    container: 'map', 
    style: 'mapbox://styles/josephda/clpdd98ft004l01re55y11ud0', // custum style URL
    center: [-122, 37], 
    zoom: 5, 
    minZoom: 5,
    maxBounds: bounds
});


const sidebar = L.control.sidebar("sidebar", { position: "left" }).addTo(map);
sidebar.open("home");

let hoveredPolygonId = null;

map.on('load', () => {
    // add geojson source data
    map.addSource('data', {
        'type': 'geojson',
        'data': 'data/final_2025.geojson',
        'generateId': true
    });
    map.addSource('counties', {
        'type': 'geojson',
        'data': 'data/counties.geojson',
    });

    // add layers
    map.addLayer({
        'id': 'Neighborhood Opportunity',
        'type': 'fill',
        'source': 'data', 
        'layout': {
            'visibility': 'visible'
        },
        'paint': {
            'fill-color': [
                'match',
                ['get', 'oppcat'],
                'Highest Resource',
                '#234EA0',
                'High Resource',
                '#2897BF',
                'Moderate Resource',
                '#73C9BD',
                'Low Resource',
                '#D5EEB3',
                '#bdbdbd'
            ],
        }
    }, 'hillshade') // place below hillshade layer

    // zoom threshold for displaying tract boundary
    const zoomThreshold = 7.75;

    map.addLayer({
        'id': 'active-layer',
        'type': 'fill',
        'source': 'data',
        'layout': {},
        'paint': {
            'fill-color': 'transparent',
            'fill-opacity': 1.0,
        }
    },
        'road-label-simple')

    // add boundary outline
    map.addLayer({
        'id': 'tract-boundary',
        'type': 'line',
        'source': 'data',
        'minzoom': zoomThreshold,
        'layout': {},
        'paint': {
            'line-color': '#000',
            'line-width': [
                'case',
                ['boolean', ['feature-state', 'hover'], false],
                3,
                0.25
            ]
        }
    },
        'road-label-simple');

    map.addLayer({
        'id': 'county-boundary',
        'type': 'line',
        'source': 'counties',
        'paint': {
            'line-color': '#000',
            'line-width': 0.75,
        }
    }, 'road-label-simple');

    let segFilter = [
        "all",
        ["in", "pov_seg_flag", 1]
    ]

    let changeFilter = [
        "all",
        ["in", "nbrhood_chng", 1]
    ]

    // add hps hatch pattern
    map.loadImage(
        'image/hps-hatch.png',
        (err, image) => {
            // throw an error if something goes wrong
            if (err) throw err;

            // add the image to the map style
            map.addImage('pattern', image);

            // create new layer and fill it
            map.addLayer({
                'id': 'High-Poverty & Segregated',
                'type': 'fill',
                'source': 'data',
                'layout': {
                    'visibility': 'visible'
                },
                'minzoom': zoomThreshold,
                'paint': {
                    'fill-pattern': 'pattern'
                },
                'filter': segFilter
            }, 'water');
        }
    );

    // add change hatch pattern
    map.loadImage(
        'image/change-hatch.png',
        (err, image) => {
            if (err) throw err;

            map.addImage('pattern2', image);

            map.addLayer({
                'id': 'Neighborhood Change',
                'type': 'fill',
                'source': 'data',
                'layout': {
                    'visibility': 'visible'
                },
                'minzoom': zoomThreshold,
                'paint': {
                    'fill-pattern': 'pattern2'
                },
                'filter': changeFilter,
            }, 'water');
        }
    );


    // begin click event
    map.on('click', 'active-layer', (e) => {
        sidebar.open("charts");

        const properties = e.features[0].properties;

        // popup
        new mapboxgl.Popup()
            .setLngLat(e.lngLat)
            .setHTML("<b><u>Neighborhood Opportunity</u></b><br>" +
                getOppCat(
                    properties.oppcat,
                    properties.exclude_flag) +
                "<br>" +
                "<br><b><u>High-Poverty & Segregated</u></b><br>" +
                hpsFlag(
                    properties.pov_seg_flag,
                    properties.exclude_flag) +
                "<br>" +
                "<br><b><u>Neighborhood Change</u></b><br> " +
                changeFlag(
                    properties.nbrhood_chng,
                    properties.exclude_flag))
            .addTo(map);



        // pass top-level info to sidebar
        document.getElementById("top-level").innerHTML =
            "<b>Census Tract ID:</b> " +
            properties.fips +
            "<br><b>Census Block Group ID: </b>" +
            bgFlag(properties.fips_bg) +
            "<br><b>Total Population</b>: " +
            properties.total_pop.toLocaleString() +
            "<br><b>Population Density</b>: " +
            properties.pop_density.toLocaleString(undefined, {
                maximumFractionDigits: 0,
            }) +
            " people/mi<sup>2</sup>" +
            "<br><b>County: </b>" +
            properties.county_name +
            "<br><b>Region: </b>" +
            properties.region

        // pass excluded geo string to sidebar
        document.getElementById("excluded").innerHTML =
            excludeString(
                properties.fips_bg,
                properties.density_flag,
                properties.military_flag,
                properties.instit_flag
            );

        // pass top-level opportunity results to sidebar
        document.getElementById("top-level-opp").innerHTML =
            scoreString(
                properties.oppcat,
                properties.fips_bg,
                properties.oppscore,
                properties.exclude_flag,
                properties.total_valid
            );

        // pass env hazard results to sidebar
        envHazBox(properties.env_site_score, properties.oppcat);
        

        // pass top-level hps results to sidebar
        document.getElementById("top-level-hps").innerHTML = hpsString(
            properties.pov_seg_flag,
            properties.exclude_flag
        );
        // pass top-level change results to sidebar
        document.getElementById("top-level-change").innerHTML =
            changeString(
                properties.pathway1a,
                properties.pathway1b,
                properties.pathway2,
                properties.region,
                properties.exclude_flag,
                properties.college_flag
                );

        // pass baseline change results to sidebar
        baseline1aBoxRace(properties.baseline_race0022)
        baseline1aBoxInc(properties.baseline_income0022)

        baseline1bBoxRace(properties.baseline_race1322)
        baseline1bBoxInc(properties.baseline_income1322)

        proximityBox(properties.halfmile_buffer);
        baseline2BoxRace(properties.baseline_race1322);
        baseline2BoxInc(properties.baseline_income1322);

        // build charts
        function getCharts(d, options, canvas) {
            labels = [];
            names = [];
            threshNames = [];
            nbVals = [];
            threshVals = [];
            barColors = [];
            threshColors = [];
            region = [];
            data = {};
            config = {};

            let oppscore = properties.oppscore;

            // map objects from indicators into new arrays
            indicators.filter((ind) => {
                if (ind.domain == d) {
                    labels.push(ind.label);
                    names.push(ind.name);
                    threshNames.push(ind.threshName);
                }
            });
            // push indicator values into arrays
            names.forEach((element) => {
                nbVals.push(properties[element]);
            });
            threshNames.forEach((element) => {
                threshVals.push(properties[element]);
            });
            // push colors into arrays
            nbVals.forEach((val1, index) => {
                let val2 = threshVals[index];

                // arrays for bar colors 
                if (d == "pov" || d == "seg") {
                    if (val1 >= val2) {
                        barColors.push("#FF9900");
                    } else {
                        barColors.push("#bdbdbd");
                    }
                // } else if (d == "change1" || d == "change1b" || d == "change2" || d == "change3" || d == "change4") {
                } else if (d.startsWith("change")) {
                    if (val1 >= val2) {
                        barColors.push("#dd3497");
                    } else {
                        barColors.push("#bdbdbd");
                    }
                } else {
                    if (val2 == null) {
                        barColors.push("#bdbdbd");
                    } else {
                        if (val1 >= val2 && oppscore >= 8) {
                            barColors.push("#234EA0");
                        } else if (val1 >= val2 && oppscore >= 6 && oppscore <= 7) {
                            barColors.push("#2897BF");
                        } else if (val1 >= val2 && oppscore >= 4 && oppscore <= 5) {
                            barColors.push("#73C9BD");
                        } else if (val1 >= val2 && oppscore <= 3) {
                            barColors.push("#D5EEB3");
                        } else {
                            barColors.push("#bdbdbd");
                        }
                    }
                }

                // arrays for thresh colors
                if (val1 == null || val2 == null) {
                    threshColors.push('#d9d9d9')
                } else {
                    threshColors.push('#000000')
                }
            });

            // add region
            nbVals.push(properties.region)

            // create data object
            data = {
                labels: labels,

                datasets: [
                    {
                        data: nbVals,
                        backgroundColor: barColors,
                        barThickness: 12,
                        borderColor: '#000000',
                        borderWidth: 1
                    },
                    {
                        data: threshShape(threshVals),
                        backgroundColor: threshColors,
                        barThickness: 16,
                        minBarLength: 3,
                        yAxisID: "y2",
                        // keep thresh line on top
                        order: -1,
                    },
                ],
            };

            // create config object
            config = {
                type: "bar",
                data: data,
                pointHitRadius: 0,
                options: options,
            };

            // assign chart to canvas
            chart = Chart.getChart(canvas);
            if (chart) {
                chart.data.datasets[0].data = nbVals;
                chart.data.datasets[0].backgroundColor = barColors;
                chart.data.datasets[1].data = threshShape(threshVals);
                chart.data.datasets[1].backgroundColor = threshColors;
                chart.options.plugins.annotation.annotations = annoNulls(nbVals);
                chart.update();
            } else {
                new Chart(canvas, config);
            }
        };

        // re-initialize objects
        let labels = [];
        let names = [];
        let threshNames = [];
        let nbVals = [];
        let threshVals = [];
        let barColors = [];
        let data = {};
        let config = {};

        // create charts
        getCharts(d = "econ1", options = econ1Options, canvas = "econ1_canv");
        getCharts(d = "econ2", options = econ2Options, canvas = "econ2_canv");
        getCharts(d = "edu", options = eduOptions, canvas = "edu_canv");
        getCharts(d = "pov", options = povOptions, canvas = "pov_canv");
        getCharts(d = "seg", options = segOptions, canvas = "seg_canv");
        getCharts(d = "change1a_race", options = change_race_options, canvas = "change1a_race_canv");
        getCharts(d = "change1a_income", options = change_income_options, canvas = "change1a_income_canv");
        getCharts(d = "change1b_race", options = change_race_options, canvas = "change1b_race_canv");
        getCharts(d = "change1b_income", options = change_income_options, canvas = "change1b_income_canv");
        getCharts(d = "change2_race", options = change_race_options, canvas = "change2_race_canv");
        getCharts(d = "change2_income", options = change_income_options, canvas = "change2_income_canv");
        getCharts(d = "change2_gap", options = change_gap_options, canvas = "change2_gap_canv");
        getCharts(d = "change2_rent", options = change_rent_options, canvas = "change2_rent_canv");




    });

    // change the cursor to a pointer when
    // the mouse is over the opp layer
    map.on('mouseenter', 'active-layer', () => {
        map.getCanvas().style.cursor = 'pointer';
    });

    // change the cursor back to a pointer
    // when it leaves the opp layer.
    map.on('mouseleave', 'active-layer', () => {
        map.getCanvas().style.cursor = '';




    });


    // add zoom control
    const nav = new mapboxgl.NavigationControl({
        showCompass: false
    });
    map.addControl(nav, 'top-right');


    // after the last frame rendered before the map enters an "idle" state...
    map.on('idle', () => {
        // if the three layers were not added to the map, abort
        if (!map.getLayer('Neighborhood Opportunity') || !map.getLayer('High-Poverty & Segregated') || !map.getLayer('Neighborhood Change')) {
            return;
        }

        // enumerate ids of the layers
        const toggleableLayerIds = ['Neighborhood Opportunity', 'High-Poverty & Segregated', 'Neighborhood Change'];

        // set up the corresponding toggle button for each layer
        for (const id of toggleableLayerIds) {
            // skip layers that already have a button set up
            if (document.getElementById(id)) {
                continue;
            }

            // create a link
            const link = document.createElement('a');
            link.id = id;
            link.href = '#';
            link.textContent = id;
            link.className = 'active';

            // show or hide layer when the toggle is clicked
            link.onclick = function (e) {
                const clickedLayer = this.textContent;
                e.preventDefault();
                e.stopPropagation();

                const visibility = map.getLayoutProperty(
                    clickedLayer,
                    'visibility'
                );

                // toggle layer visibility by changing the layout object's visibility property
                if (visibility === 'visible') {
                    map.setLayoutProperty(clickedLayer, 'visibility', 'none');
                    this.className = '';
                } else {
                    this.className = 'active';
                    map.setLayoutProperty(
                        clickedLayer,
                        'visibility',
                        'visible'
                    );
                }
            };

            const layers = document.getElementById('menu');
            layers.appendChild(link);
        }
    });



    // create hover effect
    map.on('mousemove', 'active-layer', (e) => {
        if (e.features.length > 0) {
            if (hoveredPolygonId !== null) {
                map.setFeatureState(
                    { source: 'data', id: hoveredPolygonId },
                    { hover: false }
                );
            }
            hoveredPolygonId = e.features[0].id;
            map.setFeatureState(
                { source: 'data', id: hoveredPolygonId },
                { hover: true }
            );
        }
    });
    map.on('mouseleave', 'active-layer', () => {
        if (hoveredPolygonId !== null) {
            map.setFeatureState(
                { source: 'data', id: hoveredPolygonId },
                { hover: false }
            );
        }
        hoveredPolygonId = null;
    });



    // add geocoder
    const geocoder = new MapboxGeocoder({
        accessToken: mapboxgl.accessToken,
        mapboxgl: mapboxgl,
        placeholder: 'Search',
        // limit results to CA bounding box
        bbox: [-124.41060660766607, 32.5342307609976, -114.13445790587905, 42.00965914828148]
    });

    document.getElementById('geocoder').appendChild(geocoder.onAdd(map));


    // disable map rotation using right click + drag
    map.dragRotate.disable();

    // disable map rotation using touch rotation gesture
    map.touchZoomRotate.disableRotation();


});


// ensure pop up close buttin is accessible to screen readers by dynamically removing the "aria-hidden" attribute in Mapbox source code after it has been rendered)
document.addEventListener('DOMContentLoaded', function() {
    // function to remove aria-hidden attribute from the button
    function removeAriaHidden() {
        const closeButton = document.querySelector('.mapboxgl-popup-close-button');
        if (closeButton) {
            closeButton.removeAttribute('aria-hidden');
        }
    }

    // observe changes in the DOM to detect when the Mapbox popup is added
    const observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
            if (mutation.addedNodes.length) {
                mutation.addedNodes.forEach(function(node) {
                    if (node.classList && node.classList.contains('mapboxgl-popup')) {
                        removeAriaHidden();
                    }
                });
            }
        });
    });

    // start observing the body for changes
    observer.observe(document.body, { childList: true, subtree: true });
});



