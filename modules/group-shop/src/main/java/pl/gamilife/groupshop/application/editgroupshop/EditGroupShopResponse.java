package pl.gamilife.groupshop.application.editgroupshop;
<<<<<<<< HEAD:modules/group-shop/src/main/java/pl/gamilife/groupshop/application/editgroupshop/EditGroupShopResult.java
========

import lombok.Builder;
>>>>>>>> 4e5f1486 (refactor: init):modules/group-shop/src/main/java/pl/gamilife/groupshop/application/editgroupshop/EditGroupShopResponse.java

import java.util.UUID;


public record EditGroupShopResult(
        UUID groupShopId,
        UUID groupId,
        String name,
        String description

) {

}
