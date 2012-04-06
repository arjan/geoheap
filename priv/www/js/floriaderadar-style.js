$(function()
{
    var styledMapStyleset = [
        {
            featureType: "administrative",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "landscape.natural",
            elementType: "labels",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "landscape.man_made",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "poi.attraction",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "poi.business",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "poi.government",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "poi.medical",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "poi.place_of_worship",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "poi.school",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "poi.sports_complex",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "transit.line",
            elementType: "labels",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "transit.station.airport",
            elementType: "labels",
            stylers: [
                { lightness: 58 },
                { visibility: "off" }
            ]
        },{
            featureType: "poi.park",
            elementType: "labels",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "transit.station.airport",
            stylers: [
                { lightness: 30 }
            ]
        },{
            featureType: "transit.station.rail",
            elementType: "labels",
            stylers: [
                { visibility: "simplified" }
            ]
        },{
            featureType: "road.highway",
            elementType: "labels",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "road.highway",
            stylers: [
                { visibility: "on" },
                { saturation: -100 },
                { lightness: 60 }
            ]
        },{
            featureType: "road.arterial",
            elementType: "geometry",
            stylers: [
                { visibility: "on" },
                { hue: "#2a00ff" },
                { saturation: -100 },
                { lightness: 100 }
            ]
        },{
            featureType: "road.local",
            elementType: "geometry",
            stylers: [
                { saturation: -98 },
                { lightness: 60 }
            ]
        },{
            featureType: "road.arterial",
            elementType: "labels",
            stylers: [
                { visibility: "on" },
                { saturation: -97 },
                { lightness: 70 }
            ]
        },{
            featureType: "road.highway",
            elementType: "labels",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "road.local",
            elementType: "labels",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "transit",
            elementType: "labels",
            stylers: [
                { visibility: "simplified" }
            ]
        },{
            featureType: "poi",
            elementType: "labels",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "water",
            stylers: [
                { saturation: 100 },
                { lightness: 15 }
            ]
        },{
            featureType: "landscape",
            elementType: "labels",
            stylers: [
                { visibility: "off" }
            ]
        },{
            featureType: "landscape.man_made",
            stylers: [
                { visibility: "on" }
            ]
        },{
            featureType: "landscape.natural",
            stylers: [
                { hue: "#00ff00" },
                { lightness: -15 },
                { saturation: 5 }
            ]
        },{
            featureType: "poi.park",
            stylers: [
                { visibility: "on" },
                { hue: "#00ff00" },
                { saturation: -10 }
            ]
        },{
        },{
            featureType: "poi.park",
            elementType: "labels",
            stylers: [
                { visibility: "simplified" }
            ]
        },{
            featureType: "water",
            elementType: "labels",
            stylers: [
                { visibility: "off" }
            ]
        }
    ];
    
    window.FloriadeRadarStyle = new google.maps.StyledMapType(styledMapStyleset, {name:"Floriade Radar"});
});
