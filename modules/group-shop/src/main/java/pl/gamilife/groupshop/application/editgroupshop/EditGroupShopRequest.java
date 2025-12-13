<<<<<<<< HEAD:modules/group-shop/src/main/java/pl/gamilife/groupshop/infrastructure/web/request/EditGroupShopRequest.java
package pl.gamilife.groupshop.infrastructure.web.request;
========
package pl.gamilife.groupshop.application.editgroupshop;
>>>>>>>> 4e5f1486 (refactor: init):modules/group-shop/src/main/java/pl/gamilife/groupshop/application/editgroupshop/EditGroupShopRequest.java

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

public record EditGroupShopRequest(
        @NotNull
        @Size(max = 100)
        String name,

        @NotNull
        @Size(max = 300)
        String description
) {
}
