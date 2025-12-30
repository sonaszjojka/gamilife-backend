package pl.gamilife.api.group.dto;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;


@Builder
public record GroupMemberDto(UUID groupMemberId, GroupDto memberGroup, UUID userId, Instant joinedAt, Instant leftAt,
                             Integer groupMoney, Integer totalEarnedMoney, Boolean isAdmin) implements Serializable {


    public record GroupDto(UUID groupId) implements Serializable {
    }
}