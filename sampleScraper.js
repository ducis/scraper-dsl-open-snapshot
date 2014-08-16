var system=require('system');
if (system.args.length === 1){
	console.log('No args');
	phantom.exit();
}else{
	var u = system.args[1];
	var page = require('webpage').create()
	page.open(u, function(stat){
		if(stat === 'fail'){
			console.log('failed '+u);
		}else{
			page.injectJs('jqry.js');
			//page.includeJs('jqry.js');
			var ret = page.evaluate(function(){
				//return document.querySelector(".menucardproductname").innerText;
				var items = $("div.menucardproduct");
				return $.map(items,function(x){ 
					var i = $(x);
					var cat = i.closest("#imenucard>.menucardproducts.menucardproducts").prev();//.menucardcategorycontainer");
					return {
						//'html':i.html(),
						'formid':i.find("form").attr('id'),
						'name':i.find(".menucardproductname").html().trim(),
						'desc':i.find(".menucardproductdescription").html().trim(),
						'desc1':i.find(".productextradescription").html().trim(),
						'id_product':i.find("input[name=product]").val(),
						'id_menucat':i.find("input[name=menucat]").val(),
						'id_rest':i.find("input[name=rest]").val(),
						'price':i.find("div.menucardproductprice>a").html(),
						'price_a_id':i.find("div.menucardproductprice>a").attr('id'),
						'cat':cat.find(".menucardcategoryheader").html().trim(),
						'cat_id':cat.attr('id')
						};
				});
				//return JSON.stringify(b);
				//.map(function(x){console.log(x.innerText); return x.innerText;});
			});
			console.log(JSON.stringify(ret,null,'\t'));
		}
		phantom.exit();
	});
}
/*
//i
/div.menucardproduct/-[
						/form/@formid
						/.menucardproductname/@name
						/.menucardproductdescription/@desc
						/.productextradescription/@desc1
						/input[name=product]/@id_product
						/input[name=menucat]/@id_menucat
						/input[name=rest]/@id_rest
						/div.menucardproductprice>a/@price
						/div.menucardproductprice>a/@price_a_id
]
json field name compatability
selector generation(factor out common parts)
/div.menucardproduct/`[i
    _`closest`/#imenucard>.menucardproducts.menucardproducts/
    _-[
						/form/@formid
						/.menucardproductname/@name
						/.menucardproductdescription/@desc
						/.productextradescription/@desc1
						/input[name=product]/@id_product
						/input[name=menucat]/@id_menucat
						/input[name=rest]/@id_rest
						/div.menucardproductprice>a/@price
						/div.menucardproductprice>a/@price_a_id
    ]
]
*/
