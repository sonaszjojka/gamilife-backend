package pl.gamilife.groupshop.domain.port.repository;

import pl.gamilife.groupshop.domain.model.OwnedGroupItem;

import java.util.Optional;
import java.util.UUID;

public interface OwnedGroupItemRpository {

    OwnedGroupItem save(OwnedGroupItem ownedGroupItem);

    void deleteById(UUID ownedGroupItemId);

    Optional<OwnedGroupItem> findById(UUID ownedGroupItemId);

}
