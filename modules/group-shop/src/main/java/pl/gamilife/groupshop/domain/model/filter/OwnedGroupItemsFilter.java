package pl.gamilife.groupshop.domain.model.filter;

import java.util.UUID;

public record OwnedGroupItemsFilter(
        UUID memberId,
        Boolean isUsedUp
) {
}
