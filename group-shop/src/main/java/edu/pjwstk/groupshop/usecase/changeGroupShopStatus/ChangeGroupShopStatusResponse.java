package edu.pjwstk.groupshop.usecase.changeGroupShopStatus;

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
