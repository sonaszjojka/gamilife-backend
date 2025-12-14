package pl.gamilife.groupshop.application.changegroupshopstatus;

import java.util.UUID;


public record ChangeGroupShopStatusResult(
        UUID groupShopId,
        UUID groupId,
        String name,
        String description,
        Boolean isActive

) {

}
