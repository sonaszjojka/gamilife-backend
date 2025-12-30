package pl.gamilife.group.usecase.findmembersbyidin;

import java.util.UUID;

public record FindMembersByIdInResult(
        UUID groupMemberId,
        Integer groupMoney,
        Integer totalEarnedMoney,
        UUID userId
) {
}
