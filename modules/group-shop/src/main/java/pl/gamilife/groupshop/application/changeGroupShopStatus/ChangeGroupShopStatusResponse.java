<<<<<<<< HEAD:modules/group-shop/src/main/java/pl/gamilife/groupshop/application/changegroupshopstatus/ChangeGroupShopStatusResult.java
package pl.gamilife.groupshop.application.changegroupshopstatus;
========
package pl.gamilife.groupshop.application.changeGroupShopStatus;

import lombok.Builder;
>>>>>>>> 4e5f1486 (refactor: init):modules/group-shop/src/main/java/pl/gamilife/groupshop/application/changeGroupShopStatus/ChangeGroupShopStatusResponse.java

import java.util.UUID;


public record ChangeGroupShopStatusResult(
        UUID groupShopId,
        UUID groupId,
        String name,
        String description,
        Boolean isActive

) {

}
