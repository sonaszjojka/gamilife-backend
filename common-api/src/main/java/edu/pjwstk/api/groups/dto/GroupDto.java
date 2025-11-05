package edu.pjwstk.api.groups.dto;

import lombok.Builder;

import java.io.Serializable;
import java.util.UUID;

@Builder
public record GroupDto(UUID groupId, String joinCode, UUID adminId, Character groupCurrencySymbol, Integer membersLimit,
                       GroupTypeDto groupType) implements Serializable {
    @Builder
    public record GroupTypeDto(Integer groupTypeId, String title) implements Serializable {
    }
}