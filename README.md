[Dan Garrette]: http://cs.utexas.edu/~dhg
[Chris Dyer]: http://www.cs.cmu.edu/~cdyer/
[Jason Baldridge]: http://www.jasonbaldridge.com
[Noah A. Smith]: http://www.cs.cmu.edu/~nasmith/

# CCG Supertagging: 2014

This is the software used for the following publication:

> [Weakly-Supervised Bayesian Learning of a CCG Supertagger](http://www.cs.utexas.edu/~dhg/papers/garrette_dyer_baldridge_smith_conll2014.pdf)    
> [Dan Garrette], [Chris Dyer], [Jason Baldridge], and [Noah A. Smith]  
> In Proceedings of CoNLL 2014  


# Data setup

Put the English, Chinese, and Italian data into the following directories:

     data/ccgbank
     data/ccgbank-chinese
     data/ccgbank-italian
     
The files should be arranged as follows:

	$ ls data/ccgbank/AUTO
	00	02	04	06	08	10	12	14	16	18	20	22	24
	01	03	05	07	09	11	13	15	17	19	21	23
	$ ls data/ccgbank-chinese/AUTO
	00	02	04	06	08	10	20	22	24	26	28	30
	01	03	05	07	09	11	21	23	25	27	29	31
	$ ls data/ccgbank-italian/pro
	civil_law.pro.txt	jrc_acquis.pro.txt	newspaper.pro.txt    


# Running the experiments

First, compile the code and generate the run script:

    $ ./compile

* `$tagset` was tested with the following values: `ccgfeat`, `ctbfeat`, `tutfeat`
* `$tdcut`  was tested with the following values: `0.1`, `0.01`, `0.001`, `0.0`

1. Uniform                        

		target/start dhg.ccg.run.Conll2014Run em   $tagset  --iterations 50             --tdcut $tdcut  --trinit un  --eminit un   --ccgtrinit-comb false --ccgtrinit-catprior false

2. B08                            

		target/start dhg.ccg.run.Conll2014Run em   $tagset  --iterations 50             --tdcut $tdcut  --trinit un  --eminit un   --ccgtrinit-comb 0.95  --ccgtrinit-catprior cplx,w0.5

3. FFBS B08                       

		target/start dhg.ccg.run.Conll2014Run ffbs $tagset  --samples 200 --burnin 100  --tdcut $tdcut  --trinit un  --eminit un   --ccgtrinit-comb 0.95  --ccgtrinit-catprior cplx,w0.5

4. FFBS B08 + catgram                

		target/start dhg.ccg.run.Conll2014Run ffbs $tagset  --samples 200 --burnin 100  --tdcut $tdcut  --trinit un  --eminit un   --ccgtrinit-comb 0.95  --ccgtrinit-catprior tdipcfg,t0.6,f0.5,m0.8,w0.5,l1000

5. FFBS B08 + catgram + trinit          

		target/start dhg.ccg.run.Conll2014Run ffbs $tagset  --samples 200 --burnin 100  --tdcut $tdcut  --trinit tde --eminit un   --ccgtrinit-comb 0.95  --ccgtrinit-catprior tdipcfg,t0.6,f0.5,m0.8,w0.5,l1000

6. FFBS B08 + catgram + trinit + eminit 

		target/start dhg.ccg.run.Conll2014Run ffbs $tagset  --samples 200 --burnin 100  --tdcut $tdcut  --trinit tde --eminit tde  --ccgtrinit-comb 0.95  --ccgtrinit-catprior tdipcfg,t0.6,f0.5,m0.8,w0.5,l1000

