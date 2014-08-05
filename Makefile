try:
	runghc scrapoo.hs

watch:
	inotifywait . -rm | runghc watch.hs
	
	#inotifywait . -rm | grep 'CLOSE_WRITE,CLOSE scrapoo.hs' | perl -pe 's/.*/make/' -- It blocks!
