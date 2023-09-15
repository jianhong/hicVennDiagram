HTMLWidgets.widget({

  name: "browseVenn",
  
  type: "output",
  
  factory: function(el, width, height) {
  
    var canvas = d3.select(el);
                 
    return {
      renderValue: function(x) {
        
        var widthF = function() {
          return(+canvas.select("svg").attr("width"));
        };
        var heightF = function() {
          return(+canvas.select("svg").attr("height"));
        };
        
        // load the data
        canvas.html(x.data);
        var menu = canvas.append("input")
                 .attr("type", "button")
                 .attr("name", "export")
                 .attr("id", "export")
                 .attr("value", "exportSVG");
        
        // ondrag
        // prepare the cursor for drag
        canvas.selectAll("text")
                .on("mouseover", function() {
                    d3.select(this).style("cursor", "move"); 
                })
                .on("mouseout", function() {
                    d3.select(this).style("cursor", "default");
                });
        var dragHandler = d3.drag()
                .on("start", function () {
                    var current = d3.select(this);
                    deltaX = current.attr("x") - d3.event.x;
                    deltaY = current.attr("y") - d3.event.y;
                    current.style("cursor", "grabbing");
                })
                .on("drag", function () {
                    d3.select(this)
                        .attr("x", d3.event.x + deltaX)
                        .attr("y", d3.event.y + deltaY);
                });
        dragHandler(canvas.selectAll("text"));
        
        //ColorPicker
        var cp;//colorPicker;
        var color = "#000";
        var cpCheckAll = false;
        var ColorPicker = function (target, picked) {
          var self = this;
          var target = d3.select(target);
          var colorScale = ["#FFD300","#FFFF00","#A2F300","#00DB00","#00CD00","#00FFFF",
          "#00B7FF","#0000FF","#1449C4","#4117C7","#820AC3","#DB007C",
          "#FF0000","#FF00FF","#FF7400","#FFAA00"];//change to color blindness safe?
          var getColor = function (i) {
            return colorScale[i];
          };
          if(typeof(target.style("fill"))!="undefined"){
              console.log(target.style("fill"));
            color = target.style("fill");
          }else{
            if(typeof(target.style("stroke"))!="undefined"){
              console.log(target.style("stroke"));
              color = target.style("stroke");
            }
          }
          defaultColor = color || getColor(0);

          self.pickedColor = defaultColor;
          self.defaultPicked = function (col) {
            switch(picked){
              case 0: //text
                  target.style('fill', col);
                  break;
              case 1: //path
                  target.style('fill', col);
                  break;
              default:
                console.log(target);
            }

            color = col;
            newCP();//keep it on top
          };
          var clicked = function () {
            if(typeof(picked)=="function"){
              picked(self.pickedColor);
            }else{
              self.defaultPicked(self.pickedColor);
            }
          };

          var pie = d3.pie().sort(null);
          var arc = d3.arc().innerRadius(25).outerRadius(50);
          var currentCoor = d3.mouse(canvas.select('svg').node());
          currentCoor[0] = currentCoor[0]+50;
          currentCoor[1] = currentCoor[1]+50;
          if(currentCoor[0] > widthF() - 50){
              console.log(currentCoor);
              console.log(widthF() + ";" + heightF());
            currentCoor[0] = widthF() - 50;
          }
          if(currentCoor[1] > heightF() - 50){
            currentCoor[1] = heightF() - 50;
          }
          var newCP = function(){
            if(typeof(cp)!="undefined"){
              cp.remove();
              cp = undefined;
            }
            cp = canvas.select('svg')
            .append("g")
            .attr("width", 100)
            .attr("height", 100)
            .attr("transform", "translate(" + currentCoor[0] +  " " + currentCoor[1] + ")")
            .call(d3.drag().on("drag", function(d){//moveable;
              currentCoor = [currentCoor[0]+d3.event.dx, currentCoor[1]+d3.event.dy];
              d3.select(this)
              .style("cursor", "move")
              .attr("transform", "translate(" + currentCoor[0] +  " " + currentCoor[1] + ")");
            }).on("end", function(d){
              d3.select(this).style("cursor", "default");
            }));
            var defaultPlate = cp.append("circle")
            .style("fill", defaultColor)
            .style("stroke", "#fff")
            .style("stroke-width", 1)
            .attr("r", 10)
            .attr("cx", 45)
            .attr("cy", 45)
            .on("mouseover", function () {
              var fill = d3.select(this).style("fill");
              self.pickedColor = fill;
              plate.style("fill", fill);
            })
            .on("click", clicked);
            var blackPlate = cp.append("circle")
            .style("fill", "#000")
            .style("stroke", "#fff")
            .style("stroke-width", 1)
            .attr("r", 10)
            .attr("cx", -45)
            .attr("cy", 45)
            .on("mouseover", function () {
              var fill = target.style("fill");
              self.pickedColor = fill;
              plate.style("fill", fill);
            })
            .on("click", clicked);
            var closePlate = cp.append("g")
            .attr("width", 20)
            .attr("height", 20)
            .attr("transform", "translate(45 -45)");
            closePlate.append("circle")
            .style("fill", "#fff")
            .style("stroke", "#000")
            .style("stroke-width", 1)
            .attr("r", 10)
            .attr("cx", 0)
            .attr("cy", 0)
            .on("click", function(){
              cp.remove();
            });
            closePlate.append("text")
            .style("fill", "#000")
            .attr("x", -5)
            .attr("y", 5)
            .text("X")
            .style("cursor", "default")
            .on("click", function(){
              cp.remove();
            });

            var plate = cp.append("circle")
            .style("fill", defaultColor)
            .style("stroke", "#fff")
            .style("stroke-width", 1)
            .attr("r", 25)
            .attr("cx", 0)
            .attr("cy", 0)
            .on("click", clicked);

            var colLen = [];
            for(var i=0; i<colorScale.length; i++){
              colLen.push(1);
            }
            cp.datum(colLen)
            .selectAll("path")
            .data(pie)
            .enter()
            .append("path")
            .style("fill", function (d, i) {
              return getColor(i);
            })
            .style("stroke", "#fff")
            .style("stroke-width", 1)
            .attr("d", arc)
            .on("mouseover", function () {
              var fill = d3.select(this).style("fill");
              self.pickedColor = fill;
              plate.style("fill", fill);
            })
            .on("click", clicked);
            var frm = cp.append("foreignObject")
            .attr("x", -28)
            .attr("y", 50)
            .attr("width", 50)
            .attr("height", 20);
            var inp = frm.append("xhtml:form")
            .append("input")
            .attr("value", d3.color(defaultColor).formatHex())
            .attr("style", "width:80px;")
            .on("keypress", function(){
              // IE fix
              if (!d3.event)
              d3.event = window.event;
              var e = d3.event;
              if (e.keyCode == 13)
              {
                if (typeof(e.cancelBubble) !== 'undefined') // IE
                e.cancelBubble = true;
                if (e.stopPropagation)
                e.stopPropagation();
                e.preventDefault();
                var fill = inp.node().value;
                if(/^#(?:[0-9a-fA-F]{3}){1,2}$/.exec(fill)){
                  self.pickedColor = fill;
                  plate.style("fill", self.pickedColor);
                  clicked();
                }
              }
            });
          };
          newCP();
          return(self);
        };
        canvas.selectAll('path').on("click", function(){
            cpCheckAll = true;
            ColorPicker(this, 1);
        })
        canvas.selectAll('text').on("click", function(){
            cpCheckAll = true;
            ColorPicker(this, 0);
        })
        
        // download button
        function writeDownloadLink(){
            function fireEvent(obj,evt){
              var fireOnThis = obj;
              var evObj;
              if( document.createEvent ) {
                evObj = document.createEvent('MouseEvents');
                evObj.initEvent( evt, true, false );
                fireOnThis.dispatchEvent( evObj );
              } else if( document.createEventObject ) {
                evObj = document.createEventObject();
                fireOnThis.fireEvent( 'on' + evt, evObj );
              }
            }
            svgAsDataUri(canvas.select('svg').node(),
                    'hicVennDiagram.svg', function(uri){
                var a = document.createElement('a');
                a.href = uri;
                a.download = 'hicVennDiagram.svg';
                fireEvent(a, 'click');
            });
        }
        d3.select("#export")
          .on("click", writeDownloadLink);
      },
      
      resize: function(width, height) {
        canvas.select("svg").attr("width", width)
           .attr("height", height);
      },
      
      svg: canvas.select("svg")
    };
  }
});