package pl.gamilife.groupshop.application.editgroupshop;

import lombok.Builder;

import java.util.UUID;

@Builder
public record EditGroupShopResponse(
        UUID groupShopId,
        UUID groupId,
        String name,
        String description

) {

}
