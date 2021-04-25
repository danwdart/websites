<html lang="en-GB">
    ${ head() }
    <nav class="p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary">
        <a class="p-0 pt-1 pt-sm-0 w-sm-auto text-center text-sm-left navbar-brand" href="#intro">
            <img src="/img/header.png" style="height:32px" alt="">
            <span class="title ml-2">Dan Dart</span>
        </a>
        <div>
            <ul class="navbar-nav px-3">
                ${ pages() }
            </ul>
            $if(data.social)$
            <div class="row social-row">
                <div class="text-right social-inside">
                    $for(data.social)$
                    <a href="${it.url}" target="_blank" rel="noopener" title="${it.title}" class="social" style="color:black">
                        <i class="fa$if(it.icontype)$${it.icontype}$else$b$endif$ fa-${it.icon}"></i>
                    </a>
                    $endfor$
                </div>
            </div>
            $endif$
        </div>
    </nav>
</html>