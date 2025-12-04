package pl.gamilife.groupshop.usecase.editownedgroupitem;

import pl.gamilife.groupshop.entity.OwnedGroupItem;

public interface EditOwnedGroupItemMapper {

    EditOwnedGroupItemResponse toResponse(OwnedGroupItem ownedGroupItem);
}
