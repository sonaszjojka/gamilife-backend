package pl.gamilife.groupshop.repository.impl;

import org.springframework.stereotype.Repository;
import pl.gamilife.groupshop.entity.OwnedGroupItem;
import pl.gamilife.groupshop.repository.OwnedGroupItemRpository;
import pl.gamilife.groupshop.repository.jpa.OwnedGroupItemRepositoryJpa;

import java.util.Optional;
import java.util.UUID;

@Repository
public class OwnedGroupItemRepositoryImpl implements OwnedGroupItemRpository {
    OwnedGroupItemRepositoryJpa ownedGroupItemRepositoryJpa;

    public OwnedGroupItemRepositoryImpl(OwnedGroupItemRepositoryJpa ownedGroupItemRepositoryJpa) {
        this.ownedGroupItemRepositoryJpa = ownedGroupItemRepositoryJpa;
    }


    @Override
    public OwnedGroupItem save(OwnedGroupItem ownedGroupItem) {
        return ownedGroupItemRepositoryJpa.save(ownedGroupItem);
    }

    @Override
    public void deleteById(UUID ownedGroupItemId) {
        ownedGroupItemRepositoryJpa.deleteById(ownedGroupItemId);
    }

    @Override
    public Optional<OwnedGroupItem> findById(UUID ownedGroupItemId) {
        return ownedGroupItemRepositoryJpa.findById(ownedGroupItemId);
    }
}
