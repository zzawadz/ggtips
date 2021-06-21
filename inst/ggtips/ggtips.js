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
        var settings = $.extend({
            size: 12,
            debug: false,
            data: {points: []}
        }, options);
        window.data = settings.data;
        return this.each(function() {
            id += 1;
            var self = $(this);
            var tooltip = self.find('.ggtips-tooltip');
            if (!tooltip.length) {
                warn('GGTips: Invalid Container no element with ggtips-tooltip ' +
                     'class found');
                return;
            }
            var $container = tooltip.closest('.shiny-html-output')
                .addClass('ggtips-plot');

            if (!$container.length) {
                warn('GGTips: Invalid Container: no parent with shiny-html-output ' +
                     'class found');
                return;
            }
            var container = $container[0];
            var timer;
            var css = ':css(stroke:#000000)';
            var selector = ['circle:not(:css(fill:none))',
                            'polyline:triangle:size(' + settings.size + ')',
                            'rect:size(' + settings.size + ')',
                            'line:size(' + settings.size + ')' + css,
                            'line:size(' + settings.size + ', 0)' + css,
                            'polyline:diamond:size(' + settings.size + ')',
                            'polygon'
                            ].join(', ');

            var $svg = $container.find('svg');

            $container.proximity('unbind').proximity(selector, {
                max: settings.size * 2,
                debug: settings.debug
            }, function(e) {
                var $e = $(e.target);
                var p, box;
                var point = getPoint($svg, e.target);
                if (!point) {
                    return;
                }
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
                              return;
                           }
                        }
                    }
                }

                var data_point;
                if (settings.data.gTree) {
                    data_point = getSlicePoint($svg, point);
                    p = findData(settings.data.gTree, data_point, 0.05);
                } else {
                    data_point = getPoint($svg, point);
                    p = findData(settings.data.points, data_point, 0.05);
                }
                if (settings.debug) {
                    e.target.style.stroke = '#000000';
                }
                if (p) {
                    var offset = container.getBoundingClientRect();
                    box = e.target.getBoundingClientRect();
                    var background = $e.css('fill');
                    //default black for black & white color scheme
                    if (background === 'none') {
                        background = '#000';
                    }

                    var color = contrastColor(background);
                    $container.addClass('ggtips-show-tooltip');
                    container.style.setProperty('--color', color);
                    container.style.setProperty('--background', background);
                    tooltip.html(p.tooltip);
                    var top = box.top - (tooltip.height() / 2) +
                        (box.height / 2) - offset.top;
                    var tooltipWidth = tooltip.prop('clientWidth');
                    // 5px to compensate for ::before triangle
                    var left = box.left + box.width + 5 - offset.left;
                    var rAlign = left + tooltipWidth + 5 > $container.width();
                    if (rAlign) {
                        // 5 - triangle width
                        left = box.left - 5 - offset.left - tooltipWidth;
                    }
                    tooltip.toggleClass('ggtips-tooltip-right', rAlign);
                    tooltip.css({
                        left: left,
                        top: top
                    });
                }
            }, function(e) {
                $container.removeClass('ggtips-show-tooltip');
                if (settings.debug) {
                    e.target.style.stroke = 'none';
                }
            });
        });
    };

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
    function getPoint($svg, point) {
        if (point instanceof SVGElement || point instanceof $.fn.init) {
            var $e = $(point);
            var v = $svg.data('viewbox');
            var cx, cy;
            if ($e.is('circle')) {
                cx = +$e.attr('cx');
                cy = +$e.attr('cy');
            } else if ($e.is(':triangle')) {
                point = triangleCenter($e);
                point.coordX /= v.width;
                point.coordY /= v.height;
                return point;
            } else if ($e.is('rect,line,:diamond,polygon')) {
                var box = $e[0].getBBox();
                cx = box.x + (box.width / 2);
                cy = box.y + (box.height / 2);
            }
            if (typeof cx !== 'undefined' && typeof cy !== 'undefined') {
                return {
                    coordX: cx / v.width,
                    coordY: cy / v.height
                };
            }
        } else {
            return point;
        }
    }

    // -------------------------------------------------------------------------
    function getSlicePoint($svg, point) {
        if (point instanceof SVGElement || point instanceof $.fn.init) {
            var $e = $(point);
            var v = $svg.data('viewbox');
            if ($e.is('polygon')) {
                var center = $e.data('center');
            }
            if (typeof center !== 'undefined') {
                return {
                    coordX: center.x / v.width,
                    coordY: center.y / v.height
                };
            }
        } else {
            return point;
        }
    }

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
    function distance(a, b) {
        function square(x) {
            return x*x;
        }
        return Math.sqrt(square(a.coordX - b.coordX) +
                         square(a.coordY - b.coordY));
    }
    // -------------------------------------------------------------------------
    function findData(points_array, point, tolerance) {
        if (!point) {
            return;
        }
        var min;
        var candidate;
        for (var i = 0; i < points_array.length; ++i) {
            var points = points_array[i];
            for (var j = 0; j < points.length; j++) {
                var p = points[j];
                var d = distance(point, p);
                if (!min || min > d) {
                    candidate = p;
                    min = d;
                }
            }
        }
        if (min < tolerance) {
            return candidate;
        }
    }

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
    // source https://stackoverflow.com/a/49092130/387194
    function contrastColor(color) {
        color = splitColor(color);
        var luminance = 0.2126 * color.red + 0.7152 * color.green +
        0.0722 * color.blue;
        return (luminance > 140) ? '#000' : '#fff';
    }

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
    function rgb2hex(rgb) {
        var parts = rgb.match(/^rgb\(([^\)]+)\)$/)[1].split(/\s*,\s*/);
        return parts.map(function(n) {
            return ('00' + parseInt(n, 10).toString(16)).slice(-2);
        });
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
    // :: Proximity Plugin
    // -------------------------------------------------------------------------
    // :: plugin is executed on parent DOM node and you pass selector,
    // :: for elements that are children of DOM node, as first argument.
    // :: If you're near element matched by select it will trigger callback
    // :: passed as 2nd or 3rd argument options argument is optional
    // :: based on https://github.com/padolsey-archive/jquery.fn
    // -------------------------------------------------------------------------
    $.fn.proximity = function(selector, options, enter, leave) {
        if (arguments[0] == 'unbind') {
            return this.off('mousemove.proximity');
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
            var $this = $(this)
            var $elements = $this.find(selector);
            var $svg = $this.find('svg');
            var svg = $svg[0];
            // precalculate and cache constant data
            $svg.data('viewbox', viewBox(svg));
            $elements.each(function() {
                var self = $(this);
                var box = this.getBBox();
                var offset = self.offset();
                self.data('offset', offset);
                self.data('box', box);
                // big polygons are part of pie charts
                if (self.is('polygon')) {
                    if (box.width > max || box.height > max) {
                        self.addClass('large');
                        var points = getPoints(self);
                        self.data('points', points);
                        var point = getAveragePoint(points);
                        self.data('center', point);
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
                var distances = $elements.map(function() {
                    if (this instanceof SVGPolygonElement) {
                        var self = $(this);
                        if (self.is('.large')) {
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
                        distance: boxDistance(this, e.pageX, e.pageY),
                        element: this
                    };
                }).get();
                distances.forEach(function(data) {
                    if (!closest || data.distance < closest.distance) {
                        closest = data;
                    }
                });
                if (closest) {
                    var distance
                    if (prev.length) {
                        prev = prev.filter(function(prev) {
                            var item = distances.find(function(d) {
                                return d.element === prev;
                            })
                            if (item.distance > max) {
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

    function mutateProximityEvent(e, item, min, max) {
        var distance = item.distance;
        e.proximity = 1 - (
            distance < max ? distance < min ? 0 : distance / max : 1
        );
        e.distance = item.distance;
        e.target = item.element;
    }


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

    function createCircle(size) {
        var svgns = "http://www.w3.org/2000/svg";
        var circle = document.createElementNS(svgns, 'circle');
        circle.setAttributeNS(null, 'cx', 0);
        circle.setAttributeNS(null, 'cy', 0);
        circle.setAttributeNS(null, 'r', size);
        circle.setAttributeNS(null, 'style', 'fill: red; stroke: none');
        return circle;
    }

    function moveTo(element, x, y) {
        element.setAttributeNS(null, 'cx', x);
        element.setAttributeNS(null, 'cy', y);
    }

    function intersectRect(r1, r2) {
        var r1 = r1.getBoundingClientRect();
        var r2 = r2.getBoundingClientRect();

        return !(r2.left > r1.right ||
                 r2.right < r1.left ||
                 r2.top > r1.bottom ||
                 r2.bottom < r1.top);
    }
    // ref: https://stackoverflow.com/a/8721483/387194
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

    function length(pointA, pointB) {
        return Math.sqrt(square(pointB.x - pointA.x) + square(pointB.y - pointA.y));
    }

    function square(x) {
        return x * x;
    }

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

    function boxDistance(el, x, y) {
        el = $(el);
        // Calculate the distance from the closest edge of the element
        // to the cursor's current position

        var left, right, top, bottom, offset,
            cX, cY, dX, dY,
            distance = 0;

        offset = el.data('offset');
        left = offset.left;
        top = offset.top;
        var box = el.data('box');
        var width = box.width;
        var height = box.height;
        right = left + width;
        bottom = top + height;

        // inside, important fo geometries bigger than points
        if (x > left && x < right && y > top && y < bottom) {
            return 0;
        }

        cX = x > right ? right : x > left ? x : left;
        cY = y > bottom ? bottom : y > top ? y : top;

        dX = Math.abs( cX - x );
        dY = Math.abs( cY - y );

        return Math.sqrt( dX * dX + dY * dY );
    }
})(jQuery);
