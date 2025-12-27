package pl.gamilife.groupshop.application.getgroupitems;

import java.io.Serializable;
import java.util.UUID;

public record GetGroupItemResult(

        UUID id,
        String name,
        Integer price,
        Boolean isActive

) implements Serializable {
}
