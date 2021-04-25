<li class="nav-item">
    <input type="radio" style="display:none" name="selected" id="${label}" value="${label}" $if(default)$checked="checked"$endif$>
    <label class="mb-0" for="${label}">
        <a class="nav-link btn btn-sm"
            $if(url)$href="$url$" target="_blank"$endif$>
            ${label}
        </a>
    </label>
    $if(body)$
    <div class="page" id="${label}">
        <div class="row">
            <div class="col my-md-3">
                <small>» ${label}</small>
            </div>
        </div>
        <div class="row">
            $if(posts)$
            <div class="col-md-2 py-3 mb-3">
                <details open class="pl-2">
                    $for(postssidebar)$
                    <summary>${year}</summary>
                    <p></p>
                    <details open class="pl-2">
                        <summary>${month}</summary>
                        <p></p>
                        $for(posts)$
                        <p class="pl-2">
                            <a href="#${slug}">${title}</a>
                        </p>
                        $endfor$
                        <br>
                        <p></p>
                    </details>
                    $endfor$
                    <p></p>
                </details>
            </div>
            $endif$
            <div class="col-md-8 offset-md-2 py-3 mb-3 bg-light">
                ${body}
                $if(posts)$
                    ${posts()}
                $endif$
            </div>
        </div>
    </div>
    $endif$
</li>