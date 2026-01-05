package pl.gamilife.groupshop.domain.model.filter;

import java.util.UUID;

public record GroupItemsFilter(
        UUID groupShopId,
        Boolean isActive
) {
}
