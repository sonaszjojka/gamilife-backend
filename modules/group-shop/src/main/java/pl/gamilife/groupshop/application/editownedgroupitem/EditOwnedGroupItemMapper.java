package pl.gamilife.groupshop.application.editownedgroupitem;

import pl.gamilife.groupshop.domain.model.OwnedGroupItem;

public interface EditOwnedGroupItemMapper {

    EditOwnedGroupItemResponse toResponse(OwnedGroupItem ownedGroupItem);
}
