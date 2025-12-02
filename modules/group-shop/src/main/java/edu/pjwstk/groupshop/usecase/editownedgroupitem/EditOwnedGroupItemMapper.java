package edu.pjwstk.groupshop.usecase.editownedgroupitem;

import edu.pjwstk.groupshop.entity.OwnedGroupItem;

public interface EditOwnedGroupItemMapper {

    EditOwnedGroupItemResponse toResponse(OwnedGroupItem ownedGroupItem);
}
