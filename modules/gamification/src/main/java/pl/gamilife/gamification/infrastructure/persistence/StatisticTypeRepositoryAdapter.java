package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.StatisticType;
import pl.gamilife.gamification.domain.port.repository.StatisticTypeRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaStatisticTypeRepository;

import java.util.List;

@Repository
@AllArgsConstructor
public class StatisticTypeRepositoryAdapter implements StatisticTypeRepository {

    private final JpaStatisticTypeRepository jpaStatisticTypeRepository;

    @Override
    public List<StatisticType> findAll() {
        return jpaStatisticTypeRepository.findAll();
    }
}
