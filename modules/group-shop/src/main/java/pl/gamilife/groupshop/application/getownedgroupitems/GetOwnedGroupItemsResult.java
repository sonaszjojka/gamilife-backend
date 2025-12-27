package pl.gamilife.groupshop.application.getownedgroupitems;

import pl.gamilife.groupshop.application.getgroupitems.GetGroupItemResult;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

public record GetOwnedGroupItemsResult(
        UUID id,
        UUID memberId,
        Instant isUsedUp,
        GetGroupItemResult groupItem
) implements Serializable {


}
