var S = Snap('#snap');

// var c1 = S.circle(100,100,10);
// var c2 = S.circle(100,200,10);

// var l = S.line(100,100,100,200);

function canvasClear()
{
  Snap.selectAll('#snap > :not(defs)').forEach(function(e) { e.remove() });
}

// var graph = { nodes: [{name:'12'}, {name:'b',size:23,color:'red'}, {name:'c'}, {name:'ы'}],
// 	      links: [{from:0,to:1},{from:1,to:2},{from:2,to:3}] };


var width=500;
var height=500;
var F = d3.layout.force().size([width,height]);
F.linkStrength(0.5);
F.charge(-700);

function drawGraph (graph)
{
  var linksG = S.g();
  var nodesG = S.g().attr({'font-family':'verdana'});


//S.circle(200,200,100).attr({fill:'url(#pat)'});
//S.rect(0,0,width,height).attr({fill:'url(#pat)'});

//S.circle(200,200,100);


var nodes = [];
for (var i = 0; i < graph.nodes.length; i++)
{
  var n = graph.nodes[i];
  nodes[i] = S.g();
  var c = S.circle(0,0,n.size||17).attr({'class':'c'});
  var t = S.text(0,0,n.name).attr({'text-anchor':'middle', 'alignment-baseline':'middle'});
  nodes[i].append(c).append(t);
  nodes[i].name = n.name;
//  nodes[i].drag();

  nodesG.append(nodes[i]);
}


var links = [];
for (var i = 0; i < graph.links.length; i++)
{
  links[i] = S.line(0,0,10,10);
  var l = graph.links[i];
  links[i].source = nodes[l.from], links[i].target = nodes[l.to];
//  links[i].drag();

  linksG.append(links[i]);
}


F.nodes(nodes);
F.links(links);

F.on('tick', function() {
  //console.log(a);
  nodes.map (function(n) { n.attr({transform: 'translate('+n.x+','+n.y+')'}) });
  links.map (function(l) { l.attr({x1:l.source.x, y1:l.source.y,
				   x2:l.target.x, y2:l.target.y}) });

});


F.drag();
F.start();
}


function newInput ()
{
  var txt = d3.select('textarea').property('value');
  console.log(txt);

  d3.json('/vac')
    .post(txt,
	  function (a,b,c) {
	    console.log('ajax', a,b,c);
	    canvasClear();
	    drawGraph(b);
	  });
}

newInput();
