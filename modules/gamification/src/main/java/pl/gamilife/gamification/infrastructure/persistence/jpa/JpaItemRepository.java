package pl.gamilife.gamification.infrastructure.persistence.jpa;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.gamification.domain.model.Item;

import java.util.Optional;
import java.util.UUID;

public interface JpaItemRepository extends JpaRepository<Item, UUID>,JpaSpecificationExecutor<Item> {


    @Override
    @EntityGraph(attributePaths = { "itemSlot", "rarity"})
    Page<Item> findAll(Specification<Item> build, Pageable pageable);

    @Override
    @EntityGraph(attributePaths = { "itemSlot", "rarity"})
    Optional<Item> findById(UUID id);
}