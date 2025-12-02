package pl.gamilife.gamification.usecase.getallitemrarities;

import pl.gamilife.gamification.repository.RarityRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class GetAllItemRaritiesUseCaseImpl implements GetAllItemRaritiesUseCase {

    private final RarityRepository rarityRepository;

    @Override
    public GetAllItemRaritiesResult execute(GetAllItemRaritiesCommand cmd) {
        return new GetAllItemRaritiesResult(
                rarityRepository.findAll().stream()
                        .map(rarity -> new GetAllItemRaritiesResult.ItemRarityDTO(
                                rarity.getId(),
                                rarity.getName()
                        ))
                        .toList()
        );
    }
}
