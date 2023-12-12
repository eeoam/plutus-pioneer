import * as L from "https://deno.land/x/lucid@0.9.8/mod.ts";
import { assert } from "https://deno.land/std@0.90.0/testing/asserts.ts";
import * as fc from 'https://cdn.skypack.dev/fast-check';

// the script that we are testing.
const negativeRTimedValidator : L.SpendingValidator = {
    type: "PlutusV2",
    script: "590aa6590aa30100003232323322323233223232323232323232332232323232323232323332223232322322232323253350011026132632024335738921187363726970742076616c69646174696f6e206661696c65640002632323253355335333573466e24009200002702615335323232350022235002223500522350022253335333501600b00600215335001153350051333501500b00300710311333501500b00300710311333501500b003007353500322002222222222222005335011335013350240040283350125023028123333333300122333573466e1c0080040a80a4894cd4ccd5cd19b8700200102a029101215335333573466e240080040a80a44040404488ccd5cd19b8800200102a02922333573466e240080040a80a488ccd5cd19b8900200102902a22333573466e200080040a40a8894cd4ccd5cd19b8900200102a02910011002225335333573466e240080040a80a440084004409c4cd5ce248114646561646c696e65206e6f7420726561636865640002610261027133573892011c65787065637465642061206e656761746976652072656465656d6572000263333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4084088d5d0a80619a8108111aba1500b33502102335742a014666aa04aeb94090d5d0a804999aa812bae502435742a01066a0420586ae85401cccd540940b5d69aba150063232323333573466e1cd55cea80124000466a0446464646666ae68cdc39aab9d5002480008cd40a0cd40ddd69aba15002303a357426ae8940088c98c80f0cd5ce01f81f01d09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81399a81bbad35742a00460746ae84d5d1280111931901e19ab9c03f03e03a135573ca00226ea8004d5d09aba2500223263203833573807607406c26aae7940044dd50009aba1500533502175c6ae854010ccd540940a48004d5d0a801999aa812bae200135742a00460566ae84d5d1280111931901a19ab9c037036032135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860366ae84d5d1280211931901319ab9c029028024375a00a6666ae68cdc39aab9d5005480008dd69aba135573ca00c464c6404866ae7009c0980884d55cf280089baa0011375400224446a004446a00644a666a666a01000e0080042a66a0062002204820462048244464646464a666a00c42a666a00c42a666a0104260089309801a4c2a666a00e4260089309801a4c201a20162a666a00e4260089309801a4c2a666a00c4260089309801a4c20182a666a00a42014201620122a666a00a42a666a00e42600a930980224c2a666a00c42600a930980224c201820142a666a00c42600a930980224c2a666a00a42600a930980224c20164a666a00a42a666a00e42a666a00e42666a0160140040022c2c2c20162a666a00c42a666a00c42666a0140120040022c2c2c201420124a666a00842a666a00c42a666a00c42666a0140120040022c2c2c20142a666a00a42a666a00a42666a0120100040022c2c2c201220104a666a00642a666a00a42a666a00a42666a0120100040022c2c2c20122a666a00842a666a00842666a01000e0040022c2c2c2010200e4a666a00442a666a00842a666a00842666a01000e0040022c2c2c20102a666a00642a666a00642666a00e00c0040022c2c2c200e200c246a0024444444400e2444006244400424440022442466002006004244246600200600424424660020060042464460046eb0004c8004d5406088cccd55cf80092805119a80498021aba1002300335744004030464646666ae68cdc39aab9d5002480008cc8848cc00400c008c030d5d0a80118029aba135744a004464c6402c66ae700640600504d55cf280089baa0012323232323333573466e1cd55cea8022400046666444424666600200a0080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c054d5d0a80119a80780a1aba135744a004464c6403666ae700780740644d55cf280089baa00135742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263201d33573804003e03603403226aae7540044dd50009aba1500233500b75c6ae84d5d1280111931900b99ab9c01a019015135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa80a91191999aab9f00225008233500733221233001003002300635573aa004600a6aae794008c010d5d100180b09aba100111220021221223300100400312232323333573466e1d4005200023212230020033005357426aae79400c8cccd5cd19b8750024800884880048c98c8048cd5ce00a80a00800789aab9d500113754002464646666ae68cdc3a800a400c46424444600800a600e6ae84d55cf280191999ab9a3370ea004900211909111180100298049aba135573ca00846666ae68cdc3a801a400446424444600200a600e6ae84d55cf280291999ab9a3370ea00890001190911118018029bae357426aae7940188c98c8048cd5ce00a80a00800780700689aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c8038cd5ce00880800609aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401866ae7003c0380284dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402a66ae7006005c04c04804404003c0380344d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200e33573802202001801626aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900599ab9c00e00d009008135573aa00226ea8004488c8c8cccd5cd19b87500148010940188cccd5cd19b875002480088d4020c018d5d09aab9e500423333573466e1d400d20002122200223263200c33573801e01c01401201026aae7540044dd50008909111801802089110009191999ab9a3370ea0029001100311999ab9a3370ea0049000100311931900319ab9c009008004003135573a6ea800526122002122001120014910350543100112323001001223300330020020011"
};
const negativeRTimedAddr: L.Address = await (await L.Lucid.new(undefined, "Custom")).utils.validatorToAddress(negativeRTimedValidator);

const NegativeRTimedDatum = L.Data.Object({
    deadline: L.Data.Integer()
});
type NegativeRTimedDatum = L.Data.Static<typeof NegativeRTimedDatum>;

const NegativeRTimedRedeemer = L.Data.Integer();
type NegativeRTimedRedeemer = L.Data.Static<typeof NegativeRTimedRedeemer>;

async function sendToScript(
    lucid: L.Lucid,
    userPrivKey: L.PrivateKey,
    dtm: NegativeRTimedDatum
  ): Promise<L.TxHash> {
  lucid.selectWalletFromPrivateKey(userPrivKey);
  const tx = await lucid
    .newTx()
    .payToContract(negativeRTimedAddr, { inline: L.Data.to<NegativeRTimedDatum>(dtm, NegativeRTimedDatum) }, { lovelace: 10_000_000n })
    .complete();
  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  return txHash
}

async function grabFunds(
    lucid: L.Lucid,
    emulator: L.Emulator,
    userPrivKey: L.PrivateKey,
    dtm: NegativeRTimedDatum,
    r: NegativeRTimedRedeemer
  ): Promise<L.TxHash> {
  lucid.selectWalletFromPrivateKey(userPrivKey);
  const rdm: L.Redeemer = L.Data.to<NegativeRTimedRedeemer>(r, NegativeRTimedRedeemer);
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(negativeRTimedAddr);
  const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.datum == L.Data.to<NegativeRTimedDatum>(dtm,NegativeRTimedDatum));

  if (ourUTxO && ourUTxO.length > 0) {
    const tx = await lucid
        .newTx()
        .collectFrom(ourUTxO, rdm)
        .attachSpendingValidator(negativeRTimedValidator)
        .validFrom(emulator.now())
        .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    return txHash
  }
  else throw new Error("UTxO's Expected!")
}

async function runTest(dtm: NegativeRTimedDatum, r: NegativeRTimedRedeemer, n: number) {
    const user1: L.PrivateKey = L.generatePrivateKey();
    const address1: string = await (await L.Lucid.new(undefined, "Custom")).selectWalletFromPrivateKey(user1).wallet.address();

    const user2: L.PrivateKey = L.generatePrivateKey();
    const address2: string = await(await L.Lucid.new(undefined, "Custom")).selectWalletFromPrivateKey(user2).wallet.address();

    const emulator = new L.Emulator([{ address: address1, assets: { lovelace: 10_000_000_000n } }, { address: address2, assets: { lovelace: 10_000_000_000n}}]);
    const lucid = await L.Lucid.new(emulator);

    await sendToScript(lucid, user1, dtm);

    emulator.awaitSlot(n);

    await grabFunds(lucid, emulator, user2, dtm, r);

    emulator.awaitSlot(10);

    //console.log(await emulator.getUtxos(address2));
}
//await runTest({deadline:BigInt(Date.now()+20_000*5+1000)}, -42n, 5*20+1);

// UNIT TESTS

function testSucceed(
  str: string, 
  r: bigint,
  d: bigint,
  n: number
) {
  Deno.test(str, async () => {await runTest({deadline:BigInt(Date.now())+d},r,n)})
}

async function testFails(
  str: string,
  r: bigint,
  d: bigint,
  n: number
){
  Deno.test(str, async () => {
    let errorThrown = false;
    try {
      await runTest({deadline:BigInt(Date.now())+d},r,n);
    } catch (error) {
      errorThrown = true;
    }
    assert(
      errorThrown,
      "Expected to throw an error, but it completed successfully"
    );
  })
};

testSucceed("UT: User 1 locks and user 2 takes with R = -42 after deadline; succeeds",-42n, BigInt(1000*100),120);
testSucceed("UT: User 1 locks and user 2 takes with R = 0 after deadline; succeeds",0n, BigInt(1000*100),120);

testFails("UT: User 1 locks and user 2 takes with R = 42 after deadline; fails",42n, BigInt(1000*100),120);
testFails("UT: User 1 locks and user 2 takes with R = -42 before deadline; fails",-42n, BigInt(1000*100),80);
testFails("UT: User 1 locks and user 2 takes with R = 0 before deadline; fails",-0n, BigInt(1000*100),80);
testFails("UT: User 1 locks and user 2 takes with R = 42 before deadline; fails",42n, BigInt(1000*100),80);

const dl: number = 100 * 1000;

const negativeBigIntArbitrary = fc.bigIntN(256).filter((n:bigint) => n <= 0);

const positiveBigIntArbitrary = fc.bigIntN(256).filter((n:bigint) => n > 0n);

const afterDeadlineWaits = fc.integer().filter((n: number) => n >= dl);

const beforeDeadlineWaits = fc.integer().filter((n: number) => n < dl);

Deno.test("PT: Negative redeemer after deadline always succeeds", () => {
  fc.assert(fc.asyncProperty(
    negativeBigIntArbitrary, afterDeadlineWaits, async (r: bigint, n: number) => {
      try {
        await runTest({deadline:BigInt(Date.now()+dl)},r,n);
      } catch (error) {
        console.error('Test failed for r= '+ r +' with error: ' + error.message);
        throw error
      };
    }
  ), {numRuns: 100});
});

Deno.test("PT: Positive redeemer after deadline always fails", () => {
  fc.assert(fc.asyncProperty(
    positiveBigIntArbitrary, afterDeadlineWaits, async (r: bigint, n: number) => {
      let errorThrown = false;
      try {
        await runTest({deadline:BigInt(Date.now()+dl)},r,n);
      } catch (error) {
        errorThrown = true;
      }
      assert(errorThrown, 'Test failed for r= ' + r + ' and n= '+ n);
    }
  ), {numRuns: 100});
});

Deno.test("PT: Anything before deadline always fails", () => {
  fc.assert(fc.asyncProperty(
    fc.bigIntN(256), beforeDeadlineWaits, async (r: bigint, n: number) => {
      let errorThrown = false;
      try {
        await runTest({deadline:BigInt(Date.now()+dl)},r,n);
      } catch (error) {
        errorThrown = true;
      }
      assert(errorThrown, 'Test failed for r= ' + r + ' and n= '+ n);
    }
  ), {numRuns: 100});
});