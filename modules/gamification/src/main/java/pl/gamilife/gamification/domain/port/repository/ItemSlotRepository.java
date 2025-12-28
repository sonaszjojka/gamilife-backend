package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.ItemSlot;

import java.util.List;

public interface ItemSlotRepository {
    List<ItemSlot> findAll();
}
