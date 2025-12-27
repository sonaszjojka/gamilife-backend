package pl.gamilife.groupshop.application.getgroupitems;

import java.util.UUID;

public record GetGroupItemsResult(

        UUID GroupItemId,
        String name,
        Integer price,
        Boolean isActive

) {
}
