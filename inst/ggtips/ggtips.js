//# sourceURL=ggtips.js
/**
 * this file is part of ggtips (R package to display tooltips on svg ggplot)
 *
 * @author Jakub Jankiewicz <https://jcubic.pl/me>
 *
 * Copyright 2018 Genentech, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 *  The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
if (typeof jQuery === 'undefined') {
    throw new Error("ggtips.js require jQuery");
}

(function($) {
    // -------------------------------------------------------------------------
    // :: GGPlot Tooltips
    // -------------------------------------------------------------------------
    var id = 0;
    $.fn.ggtips = function(options) {
        if (arguments[0] === 'unbind') {
            return this.each(function() {
                var $this = $(this);
                $this.removeClass('ggtips-plot').find('svg').proximity('unbind');
                $this.off('resize');
            });
        }

        function tolerance(type) {
            if (typeof data[type].tolerance === 'number') {
                return data[type].tolerance;
            }
            if (typeof settings.tolerance === 'number') {
                return settings.tolerance;
            }
            return settings.tolerance[type];
        }

        var settings = $.extend({
            size: 12,
            tolerance: 0.05,
            follow: false,
            debug: false
        }, options, {
            data: $.extend({
                points: {},
                rect: {},
                polar_rect: {}
            }, options && options.data)
        });

        var data = settings.data;

        return this.each(function() {
            id += 1;
            var $container = $(this).addClass('ggtips-plot');
            var $tooltip = $container.find('.ggtips-tooltip');
            if (!$tooltip.length) {
                warn('GGTips: Invalid Container no element with ggtips-tooltip ' +
                     'class found');
                return;
            }
            var container = $container[0];
            var $svg = $container.find('svg');
            $container.on('resize', function() {
                $tooltip.removeClass('ggtips-show-tooltip');
            })
            var timer;
            var css = ':css(stroke:#000000)';
            var selectors = [
                'circle',
                'polyline:triangle:size(' + settings.size + ')',
                'rect:size(' + settings.size + ')',
                'line:size(' + settings.size + ')' + css,
                'line:size(' + settings.size + ', 0)' + css,
                'polyline:diamond:size(' + settings.size + ')'
            ];
            if (data.rect.data) {
                var rects = settings.data.rect.colors.map(color => {
                    return `rect:css(fill:${color.toLowerCase()})`;
                });
                var barPlotSelector = rects.join(',');
                selectors = selectors.concat(rects);
            }
            var selector = selectors.join(',');
            $svg.parseViewBox();
            // match svg elements to data from R
            // TODO: if slow try:
            //       * sort+binary search
            //       * remove item when found
            //       * split point by trelis panel
            $svg.find(selector).each(function() {
                var $e = $(this);
                if (isElementCliped($e)) {
                    return;
                }
                var point, p;
                if (data.rect.data && barPlotSelector &&
                    $e.is(barPlotSelector) && !isBackgroundRect($e)) { // bar plot
                    point = getRectCoords($svg, $e);
                    p = findData(data.rect.data, point, tolerance('rect'));
                } else if ($e.is('polygon')) { // pie chart
                    // TODO: add pie charts
                }
                // try points, that can also be rect or polygon
                if (!p && data.points.data) {
                    point = getPoint($svg, $e);
                    p = findData(data.points.data, point, tolerance('points'));
                }
                if (p) {
                    $e.data('raw', p); // for debug
                    $e.data('tooltip', p.tooltip);
                }
            });
            if (settings.follow) {
                if (barPlotSelector) {
                    $svg.on('mousemove', barPlotSelector, function(e) {
                        $tooltip.ggTooltip({
                            container: container,
                            element: e.target,
                            box: {
                                left: e.clientX - 5,
                                top: e.clientY,
                                height: 0,
                                width: 10
                            }
                        });
                    }).on('mouseover', barPlotSelector, function(e) {
                        var $e = $(e.target);
                        var tooltip = $e.data('tooltip');
                        if (!tooltip) {
                            return;
                        }
                        $tooltip.html(tooltip).ggShotTooltip({
                            element: e.target,
                            container: container
                        });
                    }).on('mouseout', barPlotSelector, function() {
                        $tooltip.removeClass('ggtips-show-tooltip');
                    });
                }
                return;
            }

            $svg.proximity('unbind').proximity(selector, {
                max: settings.size * 2,
                debug: settings.debug
            }, function(e) {
                var $e = $(e.target);
                var tooltip = $e.data('tooltip');
                if (!tooltip) {
                    return;
                }
                if (settings.debug) {
                    e.target.style.stroke = '#000000';
                }
                var tooltipOptions = {
                    element: e.target,
                    container: container
                };
                $tooltip.html(tooltip).ggShotTooltip(tooltipOptions);
                $tooltip.ggTooltip(tooltipOptions);
            }, function(e) {
                $tooltip.removeClass('ggtips-show-tooltip');
                if (settings.debug) {
                    e.target.style.stroke = 'none';
                }
            });
        });
    };

    // -------------------------------------------------------------------------
    // :: get background for the tooltip from element. I can be stroke on shapes
    // -------------------------------------------------------------------------
    function getSVGElementBackground(element) {
        var $e = $(element);
        var fill = $e.css('fill');
        var stroke = $e.css('stroke');
        if (!(stroke === 'none' || stroke === '')) {
            return stroke;
        } else if (!(fill === 'none' || fill === '')) {
            return fill;
        } else {
            return '#000';
        }
    }

    // -------------------------------------------------------------------------
    // :: style tooltip according to given element
    // -------------------------------------------------------------------------
    $.fn.ggShotTooltip = function(options) {
        this.addClass('ggtips-show-tooltip');
        // this is slow (trigger css layout)
        var background = getSVGElementBackground(options.element);
        var color = contrastColor(background);
        var style = options.container.style;
        style.setProperty('--color', color);
        style.setProperty('--background', background);
    };

    // -------------------------------------------------------------------------
    // :: position tooltip based on container and given element
    // -------------------------------------------------------------------------
    var TRIANGLE_WIDTH = 8;
    // -------------------------------------------------------------------------
    $.fn.ggTooltip = function(options) {
        var box;
        if (options.box) {
            box = options.box;
        } else {
            box = options.element.getBoundingClientRect();
        }
        var offset = options.container.getBoundingClientRect();
        var top = 2 + box.top - (this.height() / 2) +
                           (box.height / 2) - offset.top;
        var tooltipWidth = this.prop('clientWidth');
        var left = box.left + box.width + TRIANGLE_WIDTH - offset.left;
        var $container = $(options.container);
        var rAlign = left + tooltipWidth + TRIANGLE_WIDTH > $container.width();
        if (rAlign) {
            left = box.left - TRIANGLE_WIDTH - offset.left - tooltipWidth;
            if (left < 0) {
                left = box.left + box.width;
                left = left - offset.left - tooltipWidth - TRIANGLE_WIDTH;
            }
        }
        this.toggleClass('ggtips-tooltip-right', rAlign);
        this.css({
            left: left,
            top: top
        });
    }
    // -------------------------------------------------------------------------
    // :: print warning in console or throw error if no console is available
    // -------------------------------------------------------------------------
    function warn(message) {
        if (console && console.warn) {
            console.warn(message);
        } else {
            setTimeout(function() {
                throw new Error(message);
            }, 0);
        }
    }

    // -------------------------------------------------------------------------
    // :: check if element is outside of clip-path
    // -------------------------------------------------------------------------
    function isElementCliped(element) {
        var $e = $(element);
        var clip = $e.attr('clip-path');
        if (clip && clip.match(/url/)) {
            clip = clip.replace(/url\(|\)/ig, '')
                       .replace(/[#"]/g, '');
            var root = $e.closest('svg');
            var rect = root.find('[id="' + clip + '"]').find('rect');
            if (rect.length) {
                var clientRect = rect[0].getBoundingClientRect();
                // Firefox has some issues with getting non-zero rect dimensions
                if (clientRect.width > 0 && clientRect.height > 0) {
                    p = $e[0].getBBox();
                    box = rect[0].getBBox();
                    var margin = 2; // 2px
                    if (p.x > box.x + box.width - margin ||
                        p.x < box.x + margin ||
                        p.y > box.y + box.height - margin ||
                        p.y < box.y + margin) {
                        return true;
                    }
                }
            }
        }
    }

    // -------------------------------------------------------------------------
    // :: return center of trianagle, assume that triangle is polygon or polyline
    // -------------------------------------------------------------------------
    function triangleCenter(element) {
        var points = element.attr('points').split(/\s+/).filter(Boolean);
        points = points.map(function(pair) {
            pair = pair.split(',');
            return {
                x: +pair[0],
                y: +pair[1]
            };
        });
        return {
            coordX: (points[0].x + points[1].x + points[2].x) / 3,
            coordY: (points[0].y + points[1].y + points[2].y) / 3
        };
    }

    // -------------------------------------------------------------------------
    // :: function check if the rect in SVG is background and should be ingored
    // :: by tooltips processing
    // -------------------------------------------------------------------------
    function isBackgroundRect(element) {
        var $e = $(element);
        if (!$e.is('rect')) {
            return false;
        }
        var x = $e.attr('x');
        var y = $e.attr('y');
        if (!(x || y)) { // no attrs
            return true;
        }
        x = +x;
        y = +y;
        if (!(x === 0 || y === 0)) {
            return false;
        }
        var vb = $e.closest('svg').data('viewbox');
        var width = +$e.attr('width');
        var height = +$e.attr('height');
        return vb.width === width || vb.height === height;
    }

    // -------------------------------------------------------------------------
    // :: Helper higher order function used to transform the point (0-1) into
    // :: SVG coordinate, the user function use SVG viewbox for the calculation
    // -------------------------------------------------------------------------
    function widthViewbox(fn) {
        return function($svg, point) {
            if (point instanceof SVGElement || point instanceof $.fn.init) {
                var $e = $(point);
                var viewbox = $svg.data('viewbox');
                return fn($e, viewbox);
            }
            return point;
        };
    }

    // -------------------------------------------------------------------------
    // :: Transform input point (0-1) into SVG coordinate using viewbox
    // :: the element is always rect
    // -------------------------------------------------------------------------
    var getRectCoords = widthViewbox(function($element, viewbox) {
        var box = $element[0].getBBox();
        return {
            coordX: box.x / viewbox.width,
            coordY: box.y / viewbox.height
        };
    });

    // -------------------------------------------------------------------------
    // :: Transform input point (0-1) into SVG coordinate using viewbox
    // :: the input point is always very small element
    // -------------------------------------------------------------------------
    var getPoint = widthViewbox(function($e, viewbox) {
        var cx, cy;
        if ($e.is('circle')) {
            cx = +$e.attr('cx');
            cy = +$e.attr('cy');
        } else if ($e.is(':triangle')) {
            point = triangleCenter($e);
            cx = point.coordX;
            cy = point.coordY;
        } else if ($e.is('rect,line,:diamond,polygon')) {
            var box = $e[0].getBBox();
            cx = box.x + (box.width / 2);
            cy = box.y + (box.height / 2);
        }
        if (typeof cx !== 'undefined' && typeof cy !== 'undefined') {
            return {
                coordX: cx / viewbox.width,
                coordY: cy / viewbox.height
            };
        }
    });

    // -------------------------------------------------------------------------
    // :: function parse SVG viewbox and return numbers (width and height)
    // :: it's used to transform data points (0-1) into SVG coordinate
    // -------------------------------------------------------------------------
    function viewBox(svg) {
        // jQuery attr don't work with svg tag probably because of xml namespace
        // but it work on svg elements
        var viewBox = svg.getAttribute('viewBox').split(' ');
        var width = +viewBox[2];
        var height = +viewBox[3];
        return {
            height: height,
            width: width
        };
    }

    // -------------------------------------------------------------------------
    // :: function calculate euclidean distance
    // -------------------------------------------------------------------------
    var distance = (function() {
        // TODO: code for migration from coordX, coordY to x,y
        //       remove this when migration is done
        function makeGetter(val) {
            return function(object) {
                if (val in object) {
                    return object[val];
                }
                return object['coord' + val.toUpperCase()];
            };
        }
        function square(x) {
            return x * x;
        }
        var getX = makeGetter('x');
        var getY = makeGetter('y');
        return function distance(a, b) {
            return Math.sqrt(square(getX(a) - getX(b)) +
                             square(getY(a) - getY(b)))
        };
    })();
    // -------------------------------------------------------------------------
    // :: function find point that have the shortest distance using some tollerance
    // :: this function is used to match SVG point to input data from R
    // -------------------------------------------------------------------------
    function findData(points_array, point, tolerance) {
        if (!point) {
            return;
        }
        function find(p) {
            var d = distance(point, p);
            if (!min || min > d) {
                candidate = p;
                min = d;
            }
        }
        var min;
        var candidate;
        for (var i = 0; i < points_array.length; ++i) {
            var item = points_array[i];
            // TODO: remove the nested array
            if (item instanceof Array) {
                for (var j = 0; j < item.length; j++) {
                    find(item[j]);
                }
            } else {
                find(item);
            }
        }
        if (min < tolerance) {
            return candidate;
        }
    }

    // -------------------------------------------------------------------------
    // :: function convert rgb hex color into object with each value
    // -------------------------------------------------------------------------
    function splitColor(rgb) {
        if (typeof rgb === 'object') {
            return rgb;
        }
        rgb = rgb.replace(/^#/, '');
        var red, green, blue;
        if (rgb.match(/^rgb/)) {
            rgb = rgb.match(/rgb\((.*)\)/)[1].split(/\s*,\s*/).map(Number);
            red = rgb[0];
            green = rgb[1];
            blue = rgb[2];
        } else if (rgb.length === 3) {
            red = parseInt(rgb[0] + rgb[0], 16);
            green = parseInt(rgb[1] + rgb[1], 16);
            blue = parseInt(rgb[2] + rgb[2], 16);
        } else if (rgb.length === 6) {
            red = parseInt(rgb[0] + rgb[1], 16);
            green = parseInt(rgb[1] + rgb[1], 16);
            blue = parseInt(rgb[2] + rgb[2], 16);
        } else {
            throw new Error('Invalid color ' + rgb);
        }
        return {
            red: red,
            green: green,
            blue: blue
        };
    }

    // -------------------------------------------------------------------------
    // :: function cacluate best contarasting color white or black
    // :: ref: https://stackoverflow.com/a/49092130/387194
    // -------------------------------------------------------------------------
    function contrastColor(color) {
        color = splitColor(color);
        var luminance = 0.2126 * color.red + 0.7152 * color.green +
        0.0722 * color.blue;
        return (luminance > 140) ? '#000' : '#fff';
    }

    // -------------------------------------------------------------------------
    // :: function check if polyline is closed shape
    // -------------------------------------------------------------------------
    function closedPolyline(element, sides) {
        element = $(element);
        if (element.is('polyline')) {
            var points = element.attr('points').split(/\s+/).filter(Boolean);
            var matchPoint = points[0] == points.slice(-1)[0];
            return points.length === sides + 1 &&
                matchPoint || points.length === sides;
        }
        return false;
    }

    // -------------------------------------------------------------------------
    // :: function convert rgb format (used by some browsers even when hex
    // :: is used in source, like Chrome) into hex value
    // -------------------------------------------------------------------------
    function rgb2hex(rgb) {
        var parts = rgb.match(/^rgb\(([^\)]+)\)$/)[1].split(/\s*,\s*/);
        return '#' + parts.map(function(n) {
            return ('00' + parseInt(n, 10).toString(16)).slice(-2);
        }).join('');
    }

    // -------------------------------------------------------------------------
    // :: pseudo selectors used by the ggtips plugin
    // :: they are used to select different types of points that are
    // :: generated by svglite device
    // -------------------------------------------------------------------------
    $.extend($.expr[':'], {
        // pesudo selector that allow to use :css(color: red)
        css: function(element, index, meta) {
            element = $(element);
            var rules = meta[3].split(';').filter(Boolean);
            return rules.filter(function(pair) {
                pair = pair.split(/\s*:\s*/);
                var css = element.css(pair[0]);
                if (css.match(/rgb\(/)) {
                    css = rgb2hex(css);
                }
                return css === pair[1];
            }).length === rules.length;
        },
        triangle: function(element) {
            return closedPolyline(element, 3);
        },
        diamond: function(element) {
            return closedPolyline(element, 4);
        },
        size: function(element, index, meta) {
            element = $(element);
            var spec = meta[3].split(/\s*,\s*/).map(Number);
            var rect = element[0].getBoundingClientRect();
            if (rect.width === 0 && rect.height === 0) {
                return false;
            }
            if (spec.length === 1) {
                return rect.width < spec[0] && rect.height < spec[0];
            } else {
                return rect.width < spec[0] && rect.height < spec[1];
            }
        }
    });
    // -------------------------------------------------------------------------
    // :: helper plugins
    // -------------------------------------------------------------------------
    $.fn.dimension = function() {
        return $.extend(
            {},
            this[0].getBoundingClientRect(),
            directOffset(this[0])
        );
    };
    // -------------------------------------------------------------------------
    $.fn.parseViewBox = function() {
        return this.each(function() {
            var $svg = $(this);
            if (!$svg.data('viewbox')) {
                var vb = viewBox(this);
                $svg.data('viewbox', vb);
            }
        });
    };
    // -------------------------------------------------------------------------
    // :: Proximity Plugin
    // -------------------------------------------------------------------------
    // :: plugin is executed on parent DOM node and you pass selector,
    // :: for elements that are children of DOM node, as first argument.
    // :: If user cursor is near element matched by selector it will
    // :: trigger the callback passed as 2nd or 3rd argument. Options argument is optional
    // :: inspired by https://github.com/padolsey-archive/jquery.fn
    // -------------------------------------------------------------------------
    $.fn.proximity = function(selector, options, enter, leave) {
        switch(arguments[0]) {
            case 'unbind':
                return this.off('mousemove.proximity').each(function() {
                    var self = $(this);
                    var scrollHandler = self.data('scrollHandler');
                    var scrollable = self.data('scrollable');
                    if (typeof scrollHandler === 'function' &&
                        scrollable instanceof $.fn.init) {
                        scrollable.off('scroll', scrollHandler);
                        self.removeData(['scrollHandler', 'scrollable']);
                    }
                });
        }

        if (typeof options === 'function') {
            enter = options;
            leave = enter;
            options = {};
        }
        var settings = $.extend({
            min: 0,
            debug: false,
            max: 10
        }, options);

        var min = settings.min;
        var max = settings.max;

        var hasLeave = typeof leave === 'function';
        var hasEnter = typeof enter === 'function';

        return this.each(function() {
            var $this = $(this);
            // filter out data that don't was not matched
            // example legend objects or background rect
            var $elements = $this.find(selector).filter(function() {
                var $e = $(this);
                return !!$e.data('tooltip');
            });

            var $svg;
            if ($this.is('svg')) {
                $svg = $this;
            } else {
                $svg = $this.find('svg');
            }
            var svg = $svg[0];
            $svg.parseViewBox();
            $elements.each(function() {
                var self = $(this);
                var box = this.getBBox();
                var offset = directOffset(this);
                self.data('offset', offset);
                self.data('box', box);
                // big polygons are part of pie charts
                if (self.is('polygon')) {
                    if (box.width > max || box.height > max) {
                        self.addClass('ggtips-large');
                        var points = getPoints(self);
                        self.data('points', points);
                        var point = getAveragePoint(points);
                        self.data('center', point);
                    }
                } else if (self.is('rect')) {
                    if (!(box.width < max || box.height < max)) {
                        self.addClass('ggtips-large');
                    }
                    if (!isBackgroundRect(self)) {
                        self.addClass('ggtips-bar');
                    }
                }
            });
            var prev = [];
            var circle;

            if (settings.debug) {
                circle = createCircle(max);
                circle.className = 'debug';
                svg.appendChild(circle);
            }
            var outside = max * 2;
            var near = max / 2;
            $this.on('mousemove.proximity', function(e) {
                var point = mapPointToSVG(svg, e.clientX, e.clientY);
                if (circle) {
                    moveTo(circle, point.x, point.y);
                }
                var closest;
                // calculate distance to all elements matched by selector
                // and find the closest
                try {
                    var distances = $elements.map(function() {
                        var distance;
                        var self = $(this);
                        if (this instanceof SVGRectElement) {
                            if (self.is('.ggtips-large')) {
                                if (isInsideRect(this, point)) {
                                    distance = 0;
                                } else {
                                    distance = outside;
                                }
                                return {
                                    distance: distance,
                                    element: this
                                };
                            }
                        }
                        if (this instanceof SVGPolygonElement) {
                            var self = $(this);
                            if (self.is('.ggtips-large')) {
                                var center = self.data('center');
                                var points = self.data('points');
                                var distance;
                                if (isPointInPoly(points, point)) {
                                    distance = 0;
                                } else {
                                    distance = outside;
                                }
                                return {
                                    distance: distance,
                                    element: this
                                };
                            }
                        }
                        return {
                            distance: boxDistance(this, point.x, point.y),
                            element: this
                        };
                    }).get();
                } catch (e) {
                    $this.proximity('unbind');
                    throw e;
                }
                distances.forEach(function(data) {
                    if (!closest || data.distance < closest.distance) {
                        closest = data;
                    }
                });
                if (closest) {
                    var distance;
                    if (prev.length) {
                        prev = prev.filter(function(prev) {
                            var item = distances.find(function(d) {
                                return d.element === prev;
                            })
                            if (item.distance !== 0) {
                                mutateProximityEvent(e, item, min, max);
                                if (hasLeave) {
                                    leave(e);
                                }
                                return false;
                            }
                            return true;
                        });
                    }

                    mutateProximityEvent(e, closest, min, max);

                    if (e.distance < max) {
                        if (hasEnter) {
                            if (prev.indexOf(e.target) === -1) {
                                var closer = distances.find(function(d) {
                                    return d.distance < e.distance;
                                });
                                if (!closer) {
                                    prev.push(e.target);
                                    enter(e);
                                }
                            }
                        }
                    }
                }
            });
        });
    };

    // -------------------------------------------------------------------------
    function mutateProximityEvent(e, item, min, max) {
        var distance = item.distance;
        e.proximity = 1 - (
            distance < max ? distance < min ? 0 : distance / max : 1
        );
        e.distance = item.distance;
        e.target = item.element;
    }

    // -------------------------------------------------------------------------
    function dimChanged(dimA, dimB) {
        return dimA.top !== dimB.top ||
               dimA.left !== dimB.left ||
               dimA.width !== dimB.width ||
               dimA.height !== dimB.height;
    }

    // -------------------------------------------------------------------------
    function mapPointToSVG(svg, x, y) {
        var pt = svg.createSVGPoint();
        pt.x = x;
        pt.y = y;
        var matrix = svg.getScreenCTM().inverse();
        var p = pt.matrixTransform(matrix);
        return {
            x: p.x,
            y: p.y
        };
    }

    // -------------------------------------------------------------------------
    function createCircle(size) {
        var svgns = "http://www.w3.org/2000/svg";
        var circle = document.createElementNS(svgns, 'circle');
        circle.setAttributeNS(null, 'cx', 0);
        circle.setAttributeNS(null, 'cy', 0);
        circle.setAttributeNS(null, 'r', size);
        circle.setAttributeNS(null, 'style', 'fill: red; stroke: none');
        return circle;
    }

    // -------------------------------------------------------------------------
    function moveTo(element, x, y) {
        element.setAttributeNS(null, 'cx', x);
        element.setAttributeNS(null, 'cy', y);
    }

    // -------------------------------------------------------------------------
    function intersectRect(r1, r2) {
        var r1 = r1.getBoundingClientRect();
        var r2 = r2.getBoundingClientRect();

        return !(r2.left > r1.right ||
                 r2.right < r1.left ||
                 r2.top > r1.bottom ||
                 r2.bottom < r1.top);
    }

    // -------------------------------------------------------------------------
    function isInsideRect(rect, pt) {
        var box = rect.getBBox();
        var x = pt.x;
        var y = pt.y;
        return box.x <= x && x <= box.x + box.width &&
               box.y <= y && y <= box.y + box.height;
    }
    // -------------------------------------------------------------------------
    // ref: https://stackoverflow.com/a/8721483/387194
    // -------------------------------------------------------------------------
    function isPointInPoly(points, pt){
        var result = false;
        for (var i = 0, j = points.length - 1; i < points.length; j = i++) {
            if ((points[i].y > pt.y) != (points[j].y > pt.y) &&
                (pt.x < (points[j].x - points[i].x) * (pt.y - points[i].y) / (points[j].y-points[i].y) + points[i].x)) {
                result = !result;
            }
        }
        return result;
    }

    // -------------------------------------------------------------------------
    function getPoints(element) {
        var points = $(element).attr('points').split(/\s+/).filter(Boolean);
        points = points.map(function(pair) {
            pair = pair.split(',');
            return {
                x: +pair[0],
                y: +pair[1]
            };
        });
        return points;
    }

    // -------------------------------------------------------------------------
    // TODO: don't include in GGtips
    function distancePoint(r, pointA, pointB) {
        var distance = length(pointA, pointB);
        var dx = pointB.x - pointA.x;
        var dy = pointB.y - pointA.y;
        var factor = r / distance;
        var x = pointA.x + (factor * dx);
        var y = pointA.y + (factor * dy);
        return {
            x: x,
            y: y
        };
    }

    // -------------------------------------------------------------------------
    function length(pointA, pointB) {
        return Math.sqrt(square(pointB.x - pointA.x) + square(pointB.y - pointA.y));
    }

    // -------------------------------------------------------------------------
    function square(x) {
        return x * x;
    }

    // -------------------------------------------------------------------------
    function getAveragePoint(points) {
        var cx = 0, cy = 0;
        points.forEach(function(point) {
            cx += point.x;
            cy += point.y;
        });
        var len = points.length
        return {
            x: cx / len,
            y: cy / len
        };
    }

    // -------------------------------------------------------------------------
    // we use the direct offset because there's an issue in safari for .offset()
    // ref: https://stackoverflow.com/a/54792141
    // -------------------------------------------------------------------------
    function directOffset(elem) {
      rect = elem.getBoundingClientRect();
      win = elem.ownerDocument.defaultView;
      return {
        top: rect.top + win.pageYOffset,
        left: rect.left + win.pageXOffset
      };
    }
    // -------------------------------------------------------------------------
    function boxDistance(el, x, y) {
        var $el = $(el);
        //var offset = $el.data('offset');
        var box = $el.data('box');
        if (!box) {
            throw new Error("ggtips: Invlaid state. If you don't use tooltips " +
                            "please try to call ggtips('destroy') to remove the evennts");
        }
        var left = box.x;
        var top = box.y;
        var right = left + box.width;
        var bottom = top + box.height;

        // inside, important fo geometries bigger than points
        if (x > left && x < right && y > top && y < bottom) {
            return 0;
        }
        // Calculate the distance from the closest edge of the element
        // to the cursor's current position
        // ref: https://stackoverflow.com/a/18157551
        var dx = Math.max(left - x, 0, x - right);
        var dy = Math.max(top - y, 0, y - bottom);
        return Math.sqrt(dx * dx + dy * dy);
    }

    // -------------------------------------------------------------------------
    // :: function checks if element is good candidate to add scroll event
    // :: this is needed because scroll event doesn't bubble
    // -------------------------------------------------------------------------
    function isScrollable() {
        var style = getComputedStyle(this);
        var overflowX = style.getPropertyValue('overflow-x');
        var overflowY = style.getPropertyValue('overflow-y');
        var values = ['scroll', 'auto'];
        return values.includes(overflowX) || values.includes(overflowY);
    }
})(jQuery);
