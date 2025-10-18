package edu.pjwstk.common.groupsApi.dto;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;


@Builder
public record GroupMemberDto(Integer groupMemberId, GroupDto memberGroup, UUID userId, Instant joinedAt, Instant leftAt,
                             Integer groupMoney, Integer totalEarnedMoney) implements Serializable {

    public record GroupDto(UUID groupId) implements Serializable {
    }
}