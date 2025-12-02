package pl.gamilife.groupshop.repository;

import pl.gamilife.groupshop.entity.OwnedGroupItem;

import java.util.Optional;
import java.util.UUID;

public interface OwnedGroupItemRpository {

    OwnedGroupItem save(OwnedGroupItem ownedGroupItem);

    void deleteById(UUID ownedGroupItemId);

    Optional<OwnedGroupItem> findById(UUID ownedGroupItemId);

}
