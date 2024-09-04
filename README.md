[![CircleCI](https://circleci.com/gh/camsaul/camsaul.com.svg?style=svg)](https://circleci.com/gh/camsaul/camsaul.com)

Source for https://camsaul.com

What you want, when you want it!

Run it locally with:

```
sudo apt install rubygems # if needed
[sudo] gem install bundler jekyll # sudo needed depending on setup
bundle install
bundle exec jekyll serve
```

Note: These deps don't seem to work on newer versions of Ruby; change `jekyll` to `4.3.3` in the Gemfile, delete
`Gemfile.lock`, and run `bundle install` again if you run into problems.
