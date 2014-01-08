
var Paper = Snap('#snap');

function canvasClear()
{
  console.log('clear');
  Snap.selectAll('#snap > :not(defs)').forEach(function(e) { e.remove() });
  return {};
}


var width=500;
var height=500;
var Force = d3.layout.force().size([width,height]);
Force.linkStrength(function(l) {return l.strength || 0.5});
Force.charge(-700);


function hslistToJS (list)
{
  var r = [];
  for (var p = list; p[0] != 0;)
  {
    var el = p[1], tail = p[2];
    r.push(el[1]);
    p = tail;
  }

  return r;
}


function drawGraph (nodesS,linksS,fromTo,tick)
{
  console.log('draw');

  var nodes = hslistToJS(nodesS);
  var nodesF = [];

  for (var i = 0; i < nodes.length; i++)
  {
    nodesF.push({id:i});
  }

  var links = hslistToJS(linksS);
  var linksF = [];
  for (var i = 0; i < fromTo.length; i++)
  {
    var l = {};
    l.source = nodesF[fromTo[i][0]];
    l.target = nodesF[fromTo[i][1]];
    l.id = i;
    linksF.push(l);
  }

  Force.nodes(nodesF);
  Force.links(linksF);

  Force.on('tick', function() {

    //A(tick, [0]);

    nodesF.map (function(f) {
      var n=nodesF[f.id];
      //nodes[f.id].attr({transform: 'translate('+n.x+','+n.y+')'});
      A(tick, [toHS(n), [0,nodes[f.id]], 0]);

    });

    linksF.map (function(l) {
      var src = nodesF[l.source.id];
      var dst = nodesF[l.target.id];
      links[l.id].attr({x1:src.x, y1:src.y,
			x2:dst.x, y2:dst.y})
    });

  });

  Force.drag();
  Force.start();

  return {};
}


