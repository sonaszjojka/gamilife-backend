package pl.gamilife.groupshop.application.changeGroupShopStatus;

import lombok.Builder;

import java.util.UUID;

@Builder
public record ChangeGroupShopStatusResponse(
        UUID groupShopId,
        UUID groupId,
        String name,
        String description,
        Boolean isActive

) {

}
