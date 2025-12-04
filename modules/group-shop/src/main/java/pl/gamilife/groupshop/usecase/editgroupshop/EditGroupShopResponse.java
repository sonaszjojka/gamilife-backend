package pl.gamilife.groupshop.usecase.editgroupshop;

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
