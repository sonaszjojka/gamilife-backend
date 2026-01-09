package pl.gamilife.api.group.dto;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;


@Builder
public record GroupMemberDto(UUID groupMemberId, GroupDto memberGroup, UUID userId, Instant joinedAt, Instant leftAt,
                             int groupMoney, int totalEarnedMoney, boolean isAdmin) implements Serializable {


    public record GroupDto(UUID groupId, String groupName) implements Serializable {
    }
}