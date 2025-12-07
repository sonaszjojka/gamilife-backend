package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.Rarity;
import pl.gamilife.gamification.domain.port.repository.RarityRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaRarityRepository;

import java.util.List;

@Repository
@AllArgsConstructor
public class RarityRepositoryAdapter implements RarityRepository {

    private final JpaRarityRepository jpaRarityRepository;

    @Override
    public List<Rarity> findAll() {
        return jpaRarityRepository.findAll();
    }
}
