package edu.pjwstk.groupshop.repository.impl;

import edu.pjwstk.groupshop.entity.OwnedGroupItem;
import edu.pjwstk.groupshop.repository.OwnedGroupItemRpository;
import edu.pjwstk.groupshop.repository.jpa.OwnedGroupItemRepositoryJpa;
import org.springframework.stereotype.Repository;

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
