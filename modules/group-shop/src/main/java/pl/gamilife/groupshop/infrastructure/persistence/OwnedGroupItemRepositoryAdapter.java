package pl.gamilife.groupshop.infrastructure.persistence;

import org.springframework.stereotype.Repository;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRpository;
import pl.gamilife.groupshop.infrastructure.persistence.jpa.OwnedGroupItemRepositoryJpa;

import java.util.Optional;
import java.util.UUID;

@Repository
public class OwnedGroupItemRepositoryAdapter implements OwnedGroupItemRpository {
    OwnedGroupItemRepositoryJpa ownedGroupItemRepositoryJpa;

    public OwnedGroupItemRepositoryAdapter(OwnedGroupItemRepositoryJpa ownedGroupItemRepositoryJpa) {
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
