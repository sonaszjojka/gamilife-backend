package edu.pjwstk.common.groupshopApi.dto;

import lombok.Builder;

import java.util.UUID;

@Builder
public record CreateGroupShopForGroupResponseDto(
        UUID groupShopId,
        String name,
        String description

) {
}
