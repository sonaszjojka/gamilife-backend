package pl.gamilife.group.usecase.findgroupmemberbyuserid;

import java.time.Instant;
import java.util.UUID;

public record FindGroupMemberByUserIdResult(
        UUID groupMemberId,
        UUID userId,
        UUID groupId,
        Integer groupMoney,
        Integer totalEarnedMoney,
        Instant leftAt,
        Instant joinedAt
) {
}
