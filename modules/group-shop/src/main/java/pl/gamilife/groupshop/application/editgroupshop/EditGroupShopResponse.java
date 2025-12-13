<<<<<<<< HEAD:modules/group-shop/src/main/java/pl/gamilife/groupshop/application/creategroupshopforgroup/CreateGroupShopForGroupResult.java
package pl.gamilife.groupshop.application.creategroupshopforgroup;
========
package pl.gamilife.groupshop.application.editgroupshop;

import lombok.Builder;

>>>>>>>> f8aa7798 (refactor: init):modules/group-shop/src/main/java/pl/gamilife/groupshop/application/editgroupshop/EditGroupShopResponse.java
import java.util.UUID;

public record CreateGroupShopForGroupResult(

        UUID groupShopId,
        UUID groupId,
        String name,
        String description

) {
}
