package pl.gamilife.group.usecase.grantrewardstomembers;

import java.util.UUID;

public record GrantRewardsToMembersResult(
        UUID groupMemberId,
        Integer groupMoney,
        Integer totalEarnedMoney,
        UUID userId
) {
}
