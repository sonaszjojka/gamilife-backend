package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.filter.StoreItemsFilter;
import pl.gamilife.gamification.domain.port.repository.ItemRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaItemRepository;
import pl.gamilife.gamification.infrastructure.persistence.specification.StoreItemSpecificationBuilder;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class ItemRepositoryAdapter implements ItemRepository {

    private final JpaItemRepository jpaItemRepository;
    private final StoreItemSpecificationBuilder specificationBuilder;

    @Override
    public Optional<Item> findWithSlotAndRarityById(UUID itemId) {
        return jpaItemRepository.findWithSlotAndRarityById(itemId);
    }

    @Override
    public Optional<Item> findById(UUID itemId) {
        return jpaItemRepository.findById(itemId);
    }

    @Override
    public Page<Item> findAll(StoreItemsFilter filter, Integer page, Integer size) {
        org.springframework.data.domain.Page<Item> result = jpaItemRepository.findAll(
                specificationBuilder.build(filter),
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
