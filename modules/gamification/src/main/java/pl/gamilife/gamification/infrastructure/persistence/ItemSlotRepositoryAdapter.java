package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.ItemSlot;
import pl.gamilife.gamification.domain.port.repository.ItemSlotRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaItemSlotRepository;

import java.util.List;

@Repository
@AllArgsConstructor
public class ItemSlotRepositoryAdapter implements ItemSlotRepository {

    private final JpaItemSlotRepository jpaItemSlotRepository;

    @Override
    public List<ItemSlot> findAll() {
        return jpaItemSlotRepository.findAll();
    }
}
