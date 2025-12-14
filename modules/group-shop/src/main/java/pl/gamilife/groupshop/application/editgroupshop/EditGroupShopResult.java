package pl.gamilife.groupshop.application.editgroupshop;

import java.util.UUID;


public record EditGroupShopResult(
        UUID groupShopId,
        UUID groupId,
        String name,
        String description

) {

}
