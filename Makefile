try:
	runghc scrapoo.hs

watch:
	inotifywait . -rm | runghc watch.hs scrapoo.hs make

watchEx1:
	inotifywait . -rm | runghc watch.hs Example1.hs 'runghc Example1.hs'
	
	#inotifywait . -rm | grep 'CLOSE_WRITE,CLOSE scrapoo.hs' | perl -pe 's/.*/make/' -- It blocks!
