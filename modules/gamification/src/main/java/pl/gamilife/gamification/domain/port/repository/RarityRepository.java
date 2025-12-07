package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.Rarity;

import java.util.List;

public interface RarityRepository {
    List<Rarity> findAll();
}
