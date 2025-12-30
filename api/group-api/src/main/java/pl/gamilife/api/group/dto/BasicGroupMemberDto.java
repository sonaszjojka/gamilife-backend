package pl.gamilife.api.group.dto;

import java.util.UUID;

public record BasicGroupMemberDto(
        UUID groupMemberId,
        Integer groupMoney,
        Integer totalEarnedMoney,
        UUID userId
) {
}
