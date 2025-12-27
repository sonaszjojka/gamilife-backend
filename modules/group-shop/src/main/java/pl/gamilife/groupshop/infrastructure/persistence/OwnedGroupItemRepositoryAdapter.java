package pl.gamilife.groupshop.infrastructure.persistence;

import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Repository;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.model.filter.OwnedGroupItemsFilter;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRepository;
import pl.gamilife.groupshop.infrastructure.persistence.jpa.OwnedGroupItemRepositoryJpa;
import pl.gamilife.groupshop.infrastructure.persistence.specification.OwnedGroupItemSpecificationBuilder;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.Optional;
import java.util.UUID;

@Repository
public class OwnedGroupItemRepositoryAdapter implements OwnedGroupItemRepository {
    OwnedGroupItemRepositoryJpa ownedGroupItemRepositoryJpa;
    OwnedGroupItemSpecificationBuilder specificationBuilder;

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

    @Override
    public Page<OwnedGroupItem> findAllMemberItems(OwnedGroupItemsFilter filer, Integer page, Integer size)
    {
        org.springframework.data.domain.Page<OwnedGroupItem> result = ownedGroupItemRepositoryJpa.findAll(
                specificationBuilder.build(filer),
                PageRequest.of(page, size)
        );

        return new Page<>(
                result.getContent(),
                result.getTotalElements(),
                result.getTotalPages(),
                result.getNumber(),
                result.getSize()
        );
    }
}
