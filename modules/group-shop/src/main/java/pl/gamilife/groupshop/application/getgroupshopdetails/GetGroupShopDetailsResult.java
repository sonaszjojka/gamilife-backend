package pl.gamilife.groupshop.application.getgroupshopdetails;

import pl.gamilife.shared.kernel.architecture.Page;

import java.io.Serializable;
import java.util.UUID;

public record GetGroupShopDetailsResult(
        UUID id,
        String name,
        String description,
        Page<GroupShopItemDto> page
) implements Serializable {

    public record GroupShopItemDto(
            UUID id,
            String name,
            Integer price,
            Boolean isActive
    ) {
    }

}
