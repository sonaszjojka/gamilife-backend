package pl.gamilife.groupshop.application.editownedgroupitem;

import lombok.Builder;

import java.time.Instant;

@Builder
public record EditOwnedGroupItemResult(
        Instant usedAt
) {


}
